{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
--use `ghc --make Gable.hs -package liquidhaskell -package statistics -package vector`
import Language.Haskell.Liquid.Liquid (runLiquid)
import Language.Haskell.Liquid.UX.CmdLine (getOpts)
import Language.Haskell.Interpreter (runInterpreter, loadModules, setImports, getLoadedModules, setTopLevelModules, interpret, as)
import GHC.IO.Exception
--import Control.Parallel
--import Control.Monad.State
import Control.Monad
import Control.DeepSeq    -- for timing computations
import System.Process (readProcessWithExitCode)
import System.Random
import System.Posix.IO
import System.IO.Unsafe
import Data.List  
import Data.Map (Map, member, (!), size, elemAt, fromList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, fromList)
import Data.Vector (fromList, Vector)
import Data.Text (Text, pack)
import Data.Text.Internal.Search (indices)
import Debug.Trace
import Statistics.Sample (mean, stdDev)
import Statistics.Quantile (def, median, midspread)
import HFlags
import System.CPUTime
import Text.Printf

{- Properties defined using command line flags -}
defineFlag "pop_size" (10 :: Int) "Size of population"
defineFlag "generations" (10 :: Int) "Number of generations"
defineFlag "chromosome_size" (5 :: Int) "Number of values in the chromosome"
defineFlag "chromosome_range" (3 :: Int) "Range of values that the chromosome can have, 0..x"
defineFlag "num_trials" (30 :: Int) "Number of trials of GE to run"
data FitnessFunction = RefinementTypes | RefinementTypesNew | IOExamples deriving (Show, Read)
defineEQFlag "fitness_function" [| RefinementTypes :: FitnessFunction |] "FITNESS_FUNCTION" "Fitness function for the GE"
defineFlag "r:random_search" False "Use random search instead of GE"
$(return []) -- hack to fix known issue with last flag not being recognized

{-properties-}
defaultFitness = 0
--popSize = 10
--generations = 10
--chromosomeSize = 5
mutationRate = 0.3
--mutationRate = 1
crossoverRate = 0.8
--crossoverRate = 1
tournamentSize = 3
eliteSize = 2

{- "program pieces" -}
data ProgramPiece = ProgramPiece { name :: String, impl :: String, refinement :: String } deriving (Show, Eq)

{- hardcoded pieces for filter evens -}
isOddImpl = unlines [
  "condition   :: Int -> Bool",
  "condition x = x `mod` 2 /= 0"
  ]
isOddRefinement = "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}"
isOddPiece = ProgramPiece "isOdd" isOddImpl isOddRefinement

isEvenImpl = unlines [
  "condition   :: Int -> Bool",
  "condition x = x `mod` 2 == 0"
  ]
isEvenRefinement = "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}"
isEvenPiece = ProgramPiece "isEven" isEvenImpl isEvenRefinement

filterImpl = unlines [
  "filterEvens :: [Int] -> [Int]",
  "filterEvens xs = [a | a <- xs, condition a]"
  ]
filterRefinement = unlines [
  "{-@ type Even = {v:Int | v mod 2 = 0} @-}",
  "{-@ filterEvens :: [Int] -> [Even] @-}"
  ]
filterPiece = ProgramPiece "filter" filterImpl filterRefinement

-- empty piece
nullPiece = ProgramPiece "null" "" ""
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

{- Calls mutate on the population. Resets the individual since a
 change should occur. TODO (Could be smarter an verify if a reset is needed)-}
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind:pop) rndDs rndIs = (createIndiv (mutate'' (genotype ind) (take flags_chromosome_size rndDs) (take flags_chromosome_size rndIs))) : mutateOp pop (drop flags_chromosome_size rndDs) (drop flags_chromosome_size rndIs)

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
oneMax = sum

{- Combine program pieces into a string. -}
combinePieces :: [ProgramPiece] -> String
combinePieces = unlines . map impl

{- Get program piece's refinement and implementation together -}
refinementAndImpl :: ProgramPiece -> String
refinementAndImpl p = unlines [refinement p, impl p]

{- Combine program pieces along with refinements into a string. -}
combinePiecesWithRefinement :: [ProgramPiece] -> String
combinePiecesWithRefinement = unlines . map refinementAndImpl

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
  let s = combinePiecesWithRefinement (map (pieces !!) g) ++ mainPiece
  do
    writeToFilePosix synthFileName s
    cfg <- getOpts [ synthFileName ]
    (ec, _) <- runLiquid Nothing cfg
    if ec == ExitSuccess then return 1 else return 0

data RefinementErrInfo = Invalid Int | Unsafe Int deriving Eq
{- New refinement type check where we inspect stdout -}
refinementTypeCheckNew :: Genotype -> Fitness
refinementTypeCheckNew g = unsafePerformIO $ do
  let s = combinePiecesWithRefinement (map (pieces !!) g) ++ mainPiece
  do
    writeToFilePosix synthFileName s
    (ec, stdout, _) <- readProcessWithExitCode "liquid" [synthFileName] ""
    case ec of
      ExitSuccess -> return 1
      ExitFailure _ -> do
        let info = getRefinementErrInfo stdout
        let fitness = case info of
                        Invalid count -> 0.5 - fromIntegral count / fromIntegral (2 * flags_chromosome_size)
                        Unsafe count -> 1.0 - fromIntegral count / fromIntegral (2 * flags_chromosome_size)
        return fitness 

{- Check type of failure and count number of errors -}
getRefinementErrInfo :: String -> RefinementErrInfo
getRefinementErrInfo s
  | not (null (indices (pack "LIQUID: UNSAFE") text)) = Unsafe count
  | not (null (indices (pack "LIQUID: ERROR Invalid Source") text)) = Invalid count
  | otherwise = error ("unexpected error output from liquid: " ++ s)
  where text = pack s
        count = length (indices (pack "error: ") text)

{- Check input/output examples by writing to file then using eval -}
{-# NOINLINE evalIOExamples #-}
evalIOExamples :: Genotype -> Fitness
evalIOExamples g = unsafePerformIO $ do
  let s = combinePieces (map (pieces !!) g)
  writeToFilePosix synthFileName s
  r <- runInterpreter $ do
          loadModules [synthFileName]
          setImports ["Prelude"]
          modules <- getLoadedModules
          setTopLevelModules modules
          interpret "filterEvens" (as :: ([Int] -> [Int]))
  case r of
    Left err -> return 0
    Right f -> return $ fromIntegral (checkIOExamples (map f test_inputs) expected_outputs) / fromIntegral (length expected_outputs)

{- Calculate fitness & write the computation time to file -}
calculateFitnessAndTime :: Genotype -> IO Fitness
calculateFitnessAndTime g = do
  start <- getCPUTime
  let fitness = case flags_fitness_function of
                  RefinementTypes -> refinementTypeCheck g
                  RefinementTypesNew -> refinementTypeCheckNew g
                  IOExamples -> evalIOExamples g
  -- `deepseq` forces evaluation
  end <- fitness `deepseq` getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  appendFile (show flags_fitness_function ++ "_times.txt") (printf "%0.9f\n" (diff :: Double))
  return fitness

{- Calculate fitness for a genotype -}
calculateFitness :: Genotype -> Fitness
calculateFitness = 
  case flags_fitness_function of
    RefinementTypes -> refinementTypeCheck
    RefinementTypesNew -> refinementTypeCheckNew
    IOExamples -> evalIOExamples
  -- unsafePerformIO . calculateFitnessAndTime

{- Calculate the number of examples that were correct -}
checkIOExamples :: [Output] -> [Output] -> Int
checkIOExamples [] [] = 0
checkIOExamples (x:xs) (y:ys)
  -- first list is the actual output, second list is expected output
  | x == y    = 1 + checkIOExamples xs ys
  | otherwise = checkIOExamples xs ys

{- Calculate fitness on a population -}
calculateFitnessOp :: Population -> Population
calculateFitnessOp = map (\ind -> GAIndividual (genotype ind) (calculateFitness (genotype ind)))
                                       
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
evolve :: Population -> [Int] -> [Int] -> Int -> [Float] -> Population
evolve pop _ _ 0 _ = []
evolve [] _ _ _ _ = error "Empty population"
evolve pop rndVs rndIs gen rndDs = bestInd pop maxInd : evolve ( generationalReplacementOp pop ( calculateFitnessOp ( mutateOp ( xoverOp ( tournamentSelectionOp (length pop) pop rndIs tournamentSize) rndDs) rndDs rndVs) ) eliteSize) (drop (flags_pop_size * 10) rndVs) (drop (flags_pop_size * 10) rndIs) (gen - 1) (drop (flags_pop_size * 10) rndDs)

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
runTrials :: RandomGen g => Int -> g -> [Maybe Int]
runTrials 0 _ = []
runTrials n gen =
  let (g1, g2) = split gen
    in let randV = randoms' flags_chromosome_range g1 :: [Int]
           randI = randoms' flags_pop_size g1 :: [Int]
           randNumberD = randoms' 1.0 g1 :: [Float]
       in let pop = createPop flags_pop_size randV
          in let bestInds = evolve pop randV randI flags_generations randNumberD
             in let foundGen = findIndex (\x -> fitness x == 1.0) bestInds
                in trace (showPop bestInds)
                  foundGen : runTrials (n-1) g2

{- Random search: generate pop size * generations random individuals 
 - and return index / 10 of the first individual that is optimal -}
runTrialsRandomSearch :: RandomGen g => Int -> g -> [Maybe Int]
runTrialsRandomSearch 0 _ = []
runTrialsRandomSearch n gen = 
  let (g1, g2) = split gen
    in let randNumber = randoms' flags_chromosome_range g1 :: [Int]
      in let pop = createPop (flags_pop_size * flags_generations) randNumber
        in let found = findIndex (\x -> fitness x == 1.0) (calculateFitnessOp pop)
          in let gen = fmap ((+1) . (`div` flags_pop_size)) found
            in gen : runTrialsRandomSearch (n-1) g2

{- Print all the summary stats given result list -}
printSummary :: [Maybe Int] -> IO ()
printSummary vals = do
  let failed = length $ filter isNothing vals
  let reals = Data.Vector.fromList $ map (fromIntegral . fromMaybe flags_generations) vals
  print reals
  putStrLn "\nSUMMARY"
  putStrLn $ "Count: " ++ show (length vals)
  putStrLn $ "Failed: " ++ show failed
  putStrLn $ "Mean: " ++ show (mean reals)
  putStrLn $ "Median: " ++ show (median def reals)
  putStrLn $ "Min: " ++ show (minimum reals)
  putStrLn $ "Max: " ++ show (maximum reals)
  putStrLn $ "StdDev: " ++ show (stdDev reals)
  putStrLn $ "IQR: " ++ show (midspread def 4 reals)
  return ()

{- Run the GA -}
main = do
  _ <- $initHFlags ""
  gen <- getStdGen
  let vals = if flags_random_search then runTrialsRandomSearch flags_num_trials gen
             else runTrials flags_num_trials gen
  printSummary vals
