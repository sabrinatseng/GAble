{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
--use `ghc --make Gable.hs -package liquidhaskell -package statistics -package vector -package mtl`
import Language.Haskell.Liquid.Liquid (runLiquid)
import Language.Haskell.Liquid.UX.CmdLine (getOpts)
import Language.Haskell.Interpreter
import GHC.IO.Exception
--import Control.Parallel
import Control.Monad.State
import Control.Monad
import System.Random
import System.Posix.IO
import System.IO.Unsafe
import Data.List  
import Data.Map (Map, empty, insert, lookup, member, (!), size, elemAt, fromList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, fromList)
import Data.Vector (fromList, Vector)
import Debug.Trace
import Statistics.Sample (mean, stdDev)
import Statistics.Quantile (def, median, midspread)
import HFlags

{- Properties defined using command line flags -}
defineFlag "chromosome_size" (5 :: Int) "Number of values in the chromosome"
defineFlag "chromosome_range" (3 :: Int) "Range of values that the chromosome can have, 0..x"
defineFlag "num_trials" (30 :: Int) "Number of trials of GE to run"
data FitnessFunction = RefinementTypes | IOExamples deriving (Show, Read)
defineEQFlag "fitness_function" [| RefinementTypes :: FitnessFunction |] "FITNESS_FUNCTION" "Fitness function for the GE"
$(return []) -- hack to fix known issue with last flag not being recognized

{-properties-}
defaultFitness = 0
popSize = 10
generations = 10
--chromosomeSize = 5
mutationRate = 0.3
--mutationRate = 1
crossoverRate = 0.8
--crossoverRate = 1
tournamentSize = 3
eliteSize = 2

{- "program pieces" -}
data ProgramPiece = ProgramPiece { name :: String, impl :: String } deriving (Show, Eq)

{- hardcoded pieces for filter evens -}
isOddImpl = unlines [
  "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}",
  "condition   :: Int -> Bool",
  "condition x = x `mod` 2 /= 0"
  ]
isOddPiece = ProgramPiece "isOdd" isOddImpl

isEvenImpl = unlines [
  "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}",
  "condition   :: Int -> Bool",
  "condition x = x `mod` 2 == 0"
  ]
isEvenPiece = ProgramPiece "isEven" isEvenImpl

filterImpl = unlines [
  "{-@ type Even = {v:Int | v mod 2 = 0} @-}",
  "{-@ filterEvens :: [Int] -> [Even] @-}",
  "filterEvens :: [Int] -> [Int]",
  "filterEvens xs = [a | a <- xs, condition a]"
  ]
filterPiece = ProgramPiece "filter" filterImpl

-- empty piece
nullPiece = ProgramPiece "null" ""
pieces = cycle [isOddPiece, isEvenPiece, filterPiece, nullPiece]

{- input/output examples for calculating fitness -}
type Input = [Int]
type Output = [Int]
tests = [
  ([0, 1, 2, 3, 4], [0, 2, 4]),
  ([1, 3, 5, 7], []),
  ([0, 2, 4, 6], [0, 2, 4, 6]),
  ([], []),
  ([3, 2, 4, 1, 5, 9], [2, 4])
  ]
(test_inputs, expected_outputs) = unzip tests

{- options for writing to file -}
openFileFlags = OpenFileFlags { append=False, exclusive=False, noctty=False, nonBlock=False, trunc=True }
synthFileName = "synth.hs"

type Genotype = [Int]
type Fitness = Float
data GAIndividual = GAIndividual { genotype :: Genotype, fitness :: Fitness } deriving (Show, Eq)
                                            
{- Type for population-}
type Population = [GAIndividual]

{- Cache for fitness calculations -}
fitnessMemo :: Map Genotype Fitness
fitnessMemo = Data.Map.empty

{- Calls mutate on the population. Resets the individual since a
 change should occur. TODO (Could be smarter an verify if a reset is needed)-}
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind:pop) rndDs rndIs = (createIndiv (mutate'' (genotype ind) (drop (length (genotype ind)) rndDs) (drop (length (genotype ind)) rndIs))) : mutateOp pop rndDs rndIs

{- Mutate a genotype by uniformly changing the integer. -}
mutate'' :: Genotype -> [Float] -> [Int] -> [Int]
mutate'' [] _ _ = []
mutate'' _ [] _ = []
mutate'' _ _ [] = []
mutate'' (c:cs) (rndD:rndDs) (rndI:rndIs) = (if rndD > mutationRate then c else rndI) : mutate'' cs rndDs rndIs

{- Calls crossover on the population TODO How does it handle oddnumber
 sized populations? Fold? Smarter resetting values in individual TODO hardcoding rnd drop-}
xoverOp :: Population -> [Float] -> Population
xoverOp [] _ = []
xoverOp (ind1:ind2:pop) rndDs = 
  let (child1, child2) = xover (genotype ind1,genotype ind2) (take 2 rndDs)
  in (createIndiv child1): (createIndiv child2) : xoverOp pop (drop 2 rndDs)
xoverOp (ind1:[]) rndDs = [ind1]         

{- Singlepoint crossover, crossover probability is hardcoded-}
xover :: (Genotype, Genotype) -> [Float] -> (Genotype, Genotype)
xover ([],_) _ = ([],[])
xover (_,[]) _ = ([],[])
xover (_,_) [] = error "Empty rnd"
xover (p1,p2) (rndD:rndDs) =  
  if rndD < crossoverRate
     -- Remove the used random values for the rndDs for the xopoints calls
     then let xopoint1 = xopoint rndDs p1
          in (take xopoint1 p1 ++ drop xopoint1 p2, take xopoint1 p2 ++ drop xopoint1 p1)
     else (p1, p2)
          
{- Utility function for getting crossover point TODO Make nicerway of returning 1 as a minimum value -}
xopoint :: [Float] -> Genotype -> Int
xopoint [] _ = error "Empty rnd"
xopoint _ [] = error "Empty genotype" 
xopoint (rnd:rndDs) codons = max 1 (round $ (rnd) * (fromIntegral $ length codons))

{- Tournament selection on a population, counting the individuals via the cnt variable TODO Better recursion?-}
tournamentSelectionOp :: Int -> Population -> [Int] -> Int -> Population
tournamentSelectionOp 0 _ _ _ = []
tournamentSelectionOp _ [] _ _ = error "Empty population"
tournamentSelectionOp _ _ [] _ = error "Empty rnd"
tournamentSelectionOp _ _ _ 0 = error "Zero tournament size" --What about minus?
tournamentSelectionOp cnt pop rndIs tournamentSize = (bestInd (selectIndividuals rndIs pop tournamentSize) maxInd) : tournamentSelectionOp (cnt - 1) pop (drop tournamentSize rndIs) tournamentSize 

{-Selection with replacement TODO (Use parital application for tournament
selection and select individuals?-}
selectIndividuals :: [Int] -> Population -> Int -> Population
selectIndividuals _ _ 0 = [] 
selectIndividuals _ [] _ = error "Empty population"
selectIndividuals [] _ _ = error "Empty rnd"
selectIndividuals (rnd:rndIs) pop tournamentSize = (pop !! (rnd `mod` (length pop) ) ) : selectIndividuals rndIs pop (tournamentSize - 1)

{- Generational replacement with elites. TODO error catching-}
generationalReplacementOp :: Population -> Population -> Int -> Population
generationalReplacementOp orgPop newPop elites = 
  let pop = (take elites $ sortBy sortInd orgPop ) ++ (take (length newPop - elites) $ sortBy sortInd newPop )
  in --trace ("\n\n" ++ showPop orgPop ++ "\n" ++ showPop newPop ++ "\n" ++ showPop pop ++ "\n")
     pop

showInd :: GAIndividual -> String
showInd (GAIndividual genotype fitness) = "Fit:" ++ show fitness ++ ", Genotype: " ++ show genotype

showPop :: Population -> String
showPop [] = ""
showPop (ind:pop) = showInd ind ++ "\n" ++ showPop pop

{- oneMax. Counting ones-}
oneMax :: [Int] -> Int
oneMax [] = 0
oneMax (value:values) = value + oneMax values

{- Combine program pieces into a string. -}
combinePieces :: [ProgramPiece] -> String
combinePieces = unlines . (map impl)

{- Write string to synth file using posix file descriptors -}
writeToFilePosix :: String -> String -> IO ()
writeToFilePosix fname s = do
  synthFile <- openFd fname WriteOnly Nothing openFileFlags
  fdWrite synthFile s
  closeFd synthFile

{- Add a main function so refinement type check catches when something is not defined -}
mainPiece = unlines [
  "test = [1, 3, 4, 6, 7, 2]",
  "main = do",
  "        putStrLn $ \"original: \" ++ show test",
  "        putStrLn $ \"evens: \" ++ show (filterEvens test)"
  ]

{- Use refinement type checking to calculate fitness. -}
{-# NOINLINE refinementTypeCheck #-}
refinementTypeCheck :: Genotype -> Fitness
refinementTypeCheck g = unsafePerformIO $ do
  let s = (combinePieces $ map (pieces !!) g) ++ mainPiece
  do
    writeToFilePosix synthFileName s
    cfg <- getOpts [ synthFileName ]
    (ec, _) <- runLiquid Nothing cfg
    if ec == ExitSuccess then return 1 else return 0

{- Check input/output examples by writing to file then using eval -}
{-# NOINLINE evalIOExamples #-}
evalIOExamples :: Genotype -> Fitness
evalIOExamples g = unsafePerformIO $ do
  let s = (combinePieces $ map (pieces !!) g)
  writeToFilePosix synthFileName s
  r <- runInterpreter $ do
          loadModules [synthFileName]
          setImports ["Prelude"]
          modules <- getLoadedModules
          setTopLevelModules modules
          interpret "filterEvens" (as :: ([Int] -> [Int]))
  case r of
    Left err -> return 0
    Right f -> return $ (fromIntegral $ checkIOExamples (map f test_inputs) expected_outputs) / (fromIntegral $ length expected_outputs)

{- Calculate the number of examples that were correct -}
checkIOExamples :: [Output] -> [Output] -> Int
checkIOExamples [] [] = 0
checkIOExamples (x:xs) (y:ys)
  -- first list is the actual output, second list is expected output
  | x == y    = 1 + checkIOExamples xs ys
  | otherwise = checkIOExamples xs ys

{- Calculate fitness for a genotype -}
{-# NOINLINE calculateFitness #-}
calculateFitness :: Genotype -> State (Map Genotype Fitness) Fitness
calculateFitness g = do
  memo <- Control.Monad.State.get
  case Data.Map.lookup g memo of
    Nothing -> do
      -- calculate and store in cache, then return
      let fitness = case flags_fitness_function of 
                      RefinementTypes -> refinementTypeCheck g
                      IOExamples -> evalIOExamples g
      let newState = Data.Map.insert g fitness memo
      put newState
      newMemo <- Control.Monad.State.get
      return fitness
    Just fitness -> return fitness

{- Calculate fitness on a population -}
calculateFitnessOp :: Population -> State (Map Genotype Fitness) Population
calculateFitnessOp [] = return []
calculateFitnessOp (ind:pop) = do
  memo <- Control.Monad.State.get
  let (fitness, memo2) = runState (calculateFitness (genotype ind) ) memo
  let (rest, memo3) = runState (calculateFitnessOp pop) memo2
  put memo3
  return ((GAIndividual (genotype ind) fitness) : rest)
                                       
{-Makes an individual with default values-}
createIndiv :: [Int] -> GAIndividual
createIndiv [] = error "creating individual with an empty chromosome"
createIndiv xs = GAIndividual xs defaultFitness

{-creates an array of individuals with random genotypes-}
createPop :: Int -> [Int] -> Population
createPop 0 _ = []
createPop popCnt rndInts = createIndiv (take flags_chromosome_size rndInts) : createPop (popCnt-1) (drop flags_chromosome_size rndInts)
                           
{- Evolve the population recursively counting with genotype and
returning a population of the best individuals of each
generation. Hard coding tournament size and elite size TODO drop a less arbitrary value of random values than 10-}
evolve :: Population -> [Int] -> Int -> [Float] -> State (Map Genotype Fitness) Population
evolve pop _ 0 _ = return []
evolve [] _ _ _ = return (error "Empty population")
evolve pop rndIs gen rndDs = do
  memo <- Control.Monad.State.get
  let (newPop, memo2) = runState ( calculateFitnessOp ( mutateOp ( xoverOp ( tournamentSelectionOp (length pop) pop rndIs tournamentSize) rndDs) rndDs rndIs) ) memo
  let (rest, memo3) = runState ( evolve ( generationalReplacementOp pop newPop eliteSize) (drop (popSize * 10) rndIs) (gen - 1) (drop (popSize * 10) rndDs) ) memo2
  put memo3
  return (bestInd pop maxInd : rest)

{- Utility for sorting GAIndividuals in DESCENDING order-}
sortInd :: GAIndividual -> GAIndividual -> Ordering
sortInd ind1 ind2
  | fitness ind1 > fitness ind2 = LT
  | fitness ind1 < fitness ind2 = GT
  | fitness ind1 == fitness ind2 = EQ
                              
{- Utility for finding the maximum fitness in a Population-}                           
maxInd :: GAIndividual -> GAIndividual -> GAIndividual
maxInd ind1 ind2 
  | fitness ind1 > fitness ind2 = ind1
  | otherwise = ind2

{- Utility for finding the minimum fitness in a Population-}                           
minInd :: GAIndividual -> GAIndividual -> GAIndividual
minInd ind1 ind2 
  | fitness ind1 < fitness ind2 = ind1
  | otherwise = ind2
                
bestInd :: Population -> (GAIndividual -> GAIndividual -> GAIndividual) -> GAIndividual
bestInd (ind:pop) best = foldr best ind pop

{- Generates random numbers in range (0, n). -}
randoms' :: (RandomGen g, Random a, Num a) => a -> g -> [a]
randoms' n gen = let (value, newGen) = randomR (0, n) gen in value:randoms' n newGen

{- Run n trials and return list of ints representing how many
 - generations it took to find the optimal solution -}
runTrials :: RandomGen g => Int -> g -> State (Map Genotype Fitness) [Maybe Int]
runTrials 0 _ = return []
runTrials n gen = do
  memo <- Control.Monad.State.get
  let (g1, g2) = split gen
  let randNumber = randoms' flags_chromosome_range g1 :: [Int]
  let randNumberD = randoms' 1.0 g1 :: [Float]
  let pop = createPop popSize randNumber
  let (bestInds, memo2) = runState (evolve pop randNumber generations randNumberD) memo
  let foundGen = findIndex (\x -> fitness x == 1.0) bestInds
  let (rest, memo3) = runState (runTrials (n-1) g2 ) memo2
  put memo3
  return (trace (showPop bestInds) (foundGen : rest))

{- Print all the summary stats given result list -}
printSummary :: [Maybe Int] -> IO ()
printSummary vals = do
  let failed = length $ filter isNothing vals
  let reals = Data.Vector.fromList $ map fromIntegral (map (fromMaybe generations) vals)
  putStrLn $ show reals
  putStrLn $ "Count: " ++ (show $ length vals)
  putStrLn $ "Failed: " ++ (show failed)
  putStrLn $ "Mean: " ++ (show $ mean reals)
  putStrLn $ "Median: " ++ (show $ median def reals)
  putStrLn $ "Min: " ++ (show $ minimum reals)
  putStrLn $ "Max: " ++ (show $ maximum reals)
  putStrLn $ "StdDev: " ++ (show $ stdDev reals)
  putStrLn $ "IQR: " ++ (show $ midspread def 4 reals)
  return ()

{- Run the GA -}
main = do
  _ <- $initHFlags ""
  gen <- getStdGen
  let vals = evalState (runTrials flags_num_trials gen) fitnessMemo
  printSummary vals
  {--
  let pop = createPop popSize randNumber
  --putStrLn $ showPop pop
  let newPop = [createIndiv [1..10], createIndiv [1..10]]
  let bestInds = (evolve pop randNumber generations randNumberD) 
  putStrLn $ showPop bestInds
  let best = bestInd bestInds maxInd
  putStrLn $ "best: " ++ showInd best
  when ((fitness best) == 1) $ do
    let s = combinePieces $ map (pieces !!) (genotype best)
    writeToSynthFilePosix s
    putStrLn s
  --}
