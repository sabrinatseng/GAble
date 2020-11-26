--import Control.Parallel
--import Control.Monad.State
--import Monad
import System.Random
import Data.List  
import Data.Map (Map, member, (!), size, elemAt, fromList)
import Data.Set (Set, fromList)
import Debug.Trace


{-properties-}
defaultFitness = 0
popSize = 20
generations = 10
chromosomeSize = 5
mutationRate = 0.01
crossoverRate = 0.7
tournamentSize = 3
eliteSize = 2

type Genotype = [Int]
type Fitness = Int
data GAIndividual = GAIndividual { genotype :: Genotype, fitness :: Fitness } deriving (Show, Eq)
                                            
{- Type for population-}
type Population = [GAIndividual]

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
  in --trace (showPop orgPop ++ "\n" ++ showPop newPop ++ "\n" ++ showPop pop ++ "\n")
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

{- Calculate fitness for a genotype -}
calculateFitness :: Genotype -> Fitness
calculateFitness = oneMax

{- Calculate fitness on a population -}
calculateFitnessOp :: Population -> Population
calculateFitnessOp [] = []
calculateFitnessOp (ind:pop) = (GAIndividual (genotype ind) (calculateFitness (genotype ind))) : calculateFitnessOp pop
                                       
{-Makes an individual with default values-}
createIndiv :: [Int] -> GAIndividual
createIndiv [] = error "creating individual with an empty chromosome"
createIndiv xs = GAIndividual xs defaultFitness

{-creates an array of individuals with random genotypes-}
createPop :: Int -> [Int] -> Population
createPop 0 _ = []
createPop popCnt rndInts = createIndiv (take chromosomeSize rndInts) : createPop (popCnt-1) (drop chromosomeSize rndInts)
                           
{- Evolve the population recursively counting with genptype and
returning a population of the best individuals of each
generation. Hard coding tournament size and elite size TODO drop a less arbitrary value of random values than 10-}
evolve :: Population -> [Int] -> Int -> [Float] -> Population
evolve pop _ 0 _ = []
evolve [] _ _ _ = error "Empty population"
evolve pop rndIs gen rndDs = bestInd pop maxInd : evolve ( generationalReplacementOp pop ( calculateFitnessOp ( mutateOp ( xoverOp ( tournamentSelectionOp (length pop) pop rndIs tournamentSize) rndDs) rndDs rndIs) ) eliteSize) (drop (popSize * 10) rndIs) (gen - 1) (drop (popSize * 10) rndDs)

{- Utility for sorting GAIndividuals-}
sortInd :: GAIndividual -> GAIndividual -> Ordering
sortInd ind1 ind2
  | fitness ind1 > fitness ind2 = GT
  | fitness ind1 < fitness ind2 = LT
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
--randoms' :: (RandomGen g, Random a) => g -> [a]
--randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

{- Run the GA-}
main = do
  gen <- getStdGen
  let randNumber = randoms' 3 gen :: [Int]
  let randNumberD = randoms' 1.0 gen :: [Float]
  --let randNumber = randoms' gen :: [Int]
  --let randNumberD = randoms' gen :: [Float]
  let pop = createPop popSize randNumber
  let newPop = [createIndiv [1..10], createIndiv [1..10]]
  let bestInds = (evolve pop randNumber generations randNumberD) 
  putStrLn $ showInd $ bestInd bestInds maxInd
  putStrLn $ showPop bestInds
