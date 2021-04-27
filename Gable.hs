{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

--use `ghc --make Gable.hs -package liquidhaskell -package statistics -package vector -package mtl`

--import Control.Parallel

-- import Control.DeepSeq -- for timing computations
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Map (Map, elemAt, empty, fromList, insert, lookup, member, size, (!))
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, fromList)
import Data.Text (Text, pack)
import Data.Text.Internal.Search (indices)
import Data.Vector (Vector, fromList)
import Debug.Trace
import GHC.Float (float2Double)
import GHC.IO.Exception
import HFlags
import Language.Haskell.Interpreter (as, getLoadedModules, interpret, loadModules, runInterpreter, setImports, setTopLevelModules)
import Language.Haskell.Liquid.Liquid (runLiquid)
import Language.Haskell.Liquid.UX.CmdLine (getOpts)
import Statistics.Quantile (def, median, midspread)
import Statistics.Sample (mean, stdDev)
import System.CPUTime
import System.IO.Unsafe
import System.Posix.IO
import System.Posix.Files (ownerModes)
import System.Process (readProcessWithExitCode)
import System.Random
import Text.Printf

{- Properties defined using command line flags -}
defineFlag "pop_size" (10 :: Int) "Size of population"
defineFlag "generations" (10 :: Int) "Number of generations"
defineFlag "chromosome_size" (3 :: Int) "Number of values in the chromosome"
defineFlag "chromosome_range" (5 :: Int) "Range of values that the chromosome can have, 0..x"
defineFlag "num_trials" (30 :: Int) "Number of trials of GE to run"
data FitnessFunction = RefinementTypes | RefinementTypesNew | IOExamples deriving (Show, Read)
defineEQFlag "fitness_function" [| RefinementTypesNew :: FitnessFunction |] "FITNESS_FUNCTION" "Fitness function for the GE"
data Problem = FilterEvens | Bound | MultiFilter2 | MultiFilter3 | InsertionSort | InsertionSort2 | QuickSort deriving (Show, Read)
defineEQFlag "problem" [| FilterEvens :: Problem |] "PROBLEM" "Synthesis problem"
data Eval = GensToOptimal | BestFitness deriving (Show, Read)
defineEQFlag "eval" [| BestFitness :: Eval |] "EVAL" "Method for eval"
defineFlag "fname" ("synth.hs" :: String) "Filename to use for synthesis / liquid checking"
defineFlag "r:random_search" False "Use random search instead of GE"
defineFlag "f:fitness_all" False "Calculate all fitness values in the space"
defineFlag "d:disallow_dups" False "Disallow duplicates in the chromosome"
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
data ProgramPiece = ProgramPiece {name :: String, impl :: String, refinement :: String} deriving (Show, Eq)

{- hardcoded pieces for filter evens -}
isOddImpl =
  unlines
    [ "condition   :: Int -> Bool",
      "condition x = x `mod` 2 /= 0"
    ]
isOddRefinement = "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}"
isOddPiece = ProgramPiece "isOdd" isOddImpl isOddRefinement

isEvenImpl =
  unlines
    [ "condition   :: Int -> Bool",
      "condition x = x `mod` 2 == 0"
    ]
isEvenRefinement = "{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}"
isEvenPiece = ProgramPiece "isEven" isEvenImpl isEvenRefinement

filterImpl =
  unlines
    [ "filterEvens :: [Int] -> [Int]",
      "filterEvens xs = [a | a <- xs, condition a]"
    ]
filterRefinement =
  unlines
    [ "{-@ type Even = {v:Int | v mod 2 = 0} @-}",
      "{-@ filterEvens :: [Int] -> [Even] @-}"
    ]
filterPiece = ProgramPiece "filter" filterImpl filterRefinement

-- empty piece
nullPiece = ProgramPiece "null" "" ""

filterEvensPieces = cycle [isOddPiece, isEvenPiece, filterPiece, nullPiece]

{- harcoded pieces for bound -}
boundLowerImpl :: Int -> String
boundLowerImpl x =
  unlines
    [ "boundLower :: Int -> Int",
      "boundLower = max " ++ show x
    ]

boundLowerRefinement :: Int -> String
boundLowerRefinement x = "{-@ boundLower :: x:Int -> {v:Int | v >= " ++ show x ++ "} @-}"

boundLowerPiece :: Int -> ProgramPiece
boundLowerPiece x = ProgramPiece "boundLower" (boundLowerImpl x) (boundLowerRefinement x)

boundImpl =
  unlines
    [ "bound :: [Int] -> [Int]",
      "bound = map boundLower"
    ]
boundRefinement =
  unlines
    [ "{-@ type GreaterThan = {v:Int |" ++ show flags_chromosome_range ++ " <= v} @-}",
      "{-@ bound :: [Int] -> [GreaterThan] @-}"
    ]
boundPiece = ProgramPiece "bound" boundImpl boundRefinement

boundPieces = [nullPiece, boundPiece] ++ map boundLowerPiece [2 ..]

isOddImplX x =
  unlines
    [ "condition" ++ show x ++ " :: Int -> Bool",
      "condition" ++ show x ++ " x = x `mod` 2 /= 0"
    ]
isOddRefinementX x = "{-@ condition" ++ show x ++ " :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}"
isOddPieceX x = ProgramPiece ("isOdd" ++ show x) (isOddImplX x) (isOddRefinementX x)

isEvenImplX x =
  unlines
    [ "condition" ++ show x ++ " :: Int -> Bool",
      "condition" ++ show x ++ " x = x `mod` 2 == 0"
    ]
isEvenRefinementX x = "{-@ condition" ++ show x ++ " :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}"
isEvenPieceX x = ProgramPiece ("isEven" ++ show x) (isEvenImplX x) (isEvenRefinementX x)

isGTTwoImplX x =
  unlines
    [ "condition" ++ show x ++ " :: Int -> Bool",
      "condition" ++ show x ++ " x = x > 2"
    ]
isGTTwoRefinementX x = "{-@ condition" ++ show x ++ " :: x:Int -> {v:Bool | (v <=> x > 2)} @-}"
isGTTwoPieceX x = ProgramPiece ("isGTTwo" ++ show x) (isGTTwoImplX x) (isGTTwoRefinementX x)

filter1Impl = 
  unlines
    [ "filter1 :: [Int] -> [Int]",
      "filter1 xs = [x | x <- xs, condition1 x]"
    ]
filter1Refinement =
  unlines
    [ "{-@ type Even = {v:Int | v mod 2 = 0} @-}",
      "{-@ filter1 :: [Int] -> [Even] @-}"
    ]
filter1Piece = ProgramPiece "filter1" filter1Impl filter1Refinement

filter2Impl = 
  unlines
    [ "filter2 :: [Int] -> [Int]",
      "filter2 xs = [x | x <- xs, condition2 x]"
    ]
filter2Refinement =
  unlines
    [ "{-@ type Odd = {v:Int | v mod 2 /= 0} @-}",
      "{-@ filter2 :: [Int] -> [Odd] @-}"
    ]
filter2Piece = ProgramPiece "filter2" filter2Impl filter2Refinement

filter3Impl = 
  unlines
    [ "filter3 :: [Int] -> [Int]",
      "filter3 xs = [x | x <- xs, condition3 x]"
    ]
filter3Refinement =
  unlines
    [ "{-@ type GT2 = {v:Int | v > 2} @-}",
      "{-@ filter2 :: [Int] -> [GT2] @-}"
    ]
filter3Piece = ProgramPiece "filter3" filter3Impl filter3Refinement

multiFilter2Impl =
  unlines
    [ "multiFilter :: [Int] -> ([Int], [Int])",
      "multiFilter xs = (filter1 xs, filter2 xs)"
    ]
multiFilter2Refinement = "{-@ multiFilter :: [Int] -> ([Even], [Odd]) @-}"
multiFilter2Piece = ProgramPiece "multiFilter2" multiFilter2Impl multiFilter2Refinement

multiFilter3Impl =
  unlines
    [ "filter1 :: [Int] -> [Int]",
      "filter1 xs = [x | x <- xs, condition1 x]",
      "filter2 :: [Int] -> [Int]",
      "filter2 xs = [x | x <- xs, condition2 x]",
      "filter3 :: [Int] -> [Int]",
      "filter3 xs = [x | x <- xs, condition3 x]",
      "multiFilter :: [Int] -> ([Int], [Int], [Int])",
      "multiFilter xs = (filter1 xs, filter2 xs, filter3 xs)"
    ]
multiFilter3Refinement =
  unlines
    [ "{-@ type Even = {v:Int | v mod 2 = 0} @-}",
      "{-@ type Odd = {v:Int | v mod 2 /= 0} @-}",
      "{-@ type GT2 = {v:Int | v > 2} @-}",
      "{-@ filter1 :: [Int] -> [Even] @-}",
      "{-@ filter2 :: [Int] -> [Odd] @-}",
      "{-@ filter3 :: [Int] -> [GT2] @-}",
      "{-@ multiFilter :: [Int] -> ([Even], [Odd], [GT2]) @-}"
    ]
multiFilter3Piece = ProgramPiece "multiFilter3" multiFilter3Impl multiFilter3Refinement

multiFilter2Pieces = cycle [isOddPieceX 1, isEvenPieceX 1, isGTTwoPieceX 1, isOddPieceX 2, isEvenPieceX 2, isGTTwoPieceX 2, filter1Piece, filter2Piece, multiFilter2Piece, nullPiece]
multiFilter3Pieces = cycle [isOddPieceX 1, isEvenPieceX 1, isGTTwoPieceX 1, isOddPieceX 2, isEvenPieceX 2, isGTTwoPieceX 2, isOddPieceX 3, isEvenPieceX 3, isGTTwoPieceX 3, multiFilter3Piece, nullPiece]

-- pieces for insertion sort
lteConditionImplX x = unlines
  [
    "condition" ++ show x ++ ":: (Ord a) => a -> a -> Bool",
    "condition" ++ show x ++ " x y = x <= y"
  ]
lteConditionRefinementX x = "{-@ condition" ++ show x ++ " :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x <= y)} @-}"
lteConditionPieceX x = ProgramPiece ("lte" ++ show x) (lteConditionImplX x) (lteConditionRefinementX x)

gteConditionImplX x = unlines
  [
    "condition" ++ show x ++ ":: (Ord a) => a -> a -> Bool",
    "condition" ++ show x ++ " x y = x >= y"
  ]
gteConditionRefinementX x = "{-@ condition" ++ show x ++ " :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x >= y)} @-}"
gteConditionPieceX x = ProgramPiece ("gte" ++ show x) (gteConditionImplX x) (gteConditionRefinementX x)

eqConditionImplX x = unlines
  [
    "condition" ++ show x ++ ":: (Ord a) => a -> a -> Bool",
    "condition" ++ show x ++ " x y = x == y"
  ]
eqConditionRefinementX x = "{-@ condition" ++ show x ++ " :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}"
eqConditionPieceX x = ProgramPiece ("eq" ++ show x) (eqConditionImplX x) (eqConditionRefinementX x)

neqConditionImplX x = unlines
  [
    "condition" ++ show x ++ ":: (Ord a) => a -> a -> Bool",
    "condition" ++ show x ++ " x y = x /= y"
  ]
neqConditionRefinementX x = "{-@ condition" ++ show x ++ " :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x /= y)} @-}"
neqConditionPieceX x = ProgramPiece ("neq" ++ show x) (neqConditionImplX x) (neqConditionRefinementX x)

insertSortAscImpl = unlines [
  "insertSortAsc :: (Ord a) => [a] -> [a]",
  "insertSortAsc [] = []",
  "insertSortAsc (x:xs) = insertAsc x (insertSortAsc xs)"
  ]
insertSortAscRefinement = unlines [
  "{-@ type IncrList a = [a]<{\\xi xj -> xi <= xj}> @-}",
  "{-@ insertSortAsc :: (Ord a) => xs:[a] -> (IncrList a) @-}"
  ]
insertSortAscPiece = ProgramPiece "insertSortAsc" insertSortAscImpl insertSortAscRefinement

insertAscImpl = unlines [
  "insertAsc :: (Ord a) => a -> [a] -> [a]",
  "insertAsc y []     = [y]",
  "insertAsc y (x:xs)",
  " | (condition1 y x)      = y : x : xs",
  " | otherwise   = x : insertAsc y xs"
  ]
insertAscRefinement = "{-@ insertAsc :: (Ord a) => a -> IncrList a -> IncrList a @-}"
insertAscPiece = ProgramPiece "insertAsc" insertAscImpl insertAscRefinement

insertSortDecImpl = unlines [
  "insertSortDec :: (Ord a) => [a] -> [a]",
  "insertSortDec [] = []",
  "insertSortDec (x:xs) = insertDec x (insertSortDec xs)"
  ]
insertSortDecRefinement = unlines [
  "{-@ type DecrList a = [a]<{\\xi xj -> xi >= xj}> @-}",
  "{-@ insertSortDec :: (Ord a) => xs:[a] -> (DecrList a) @-}"
  ]
insertSortDecPiece = ProgramPiece "insertSortDec" insertSortDecImpl insertSortDecRefinement

insertDecImpl = unlines [
  "insertDec :: (Ord a) => a -> [a] -> [a]",
  "insertDec y []     = [y]",
  "insertDec y (x:xs)",
  " | (condition2 y x)      = y : x : xs",
  " | otherwise   = x : insertDec y xs"
  ]
insertDecRefinement = "{-@ insertDec :: (Ord a) => a -> DecrList a -> DecrList a @-}"
insertDecPiece = ProgramPiece "insertDec" insertDecImpl insertDecRefinement

sortAscDecImpl = unlines
  [
    "sortAscDec :: (Ord a) => [a] -> ([a], [a])",
    "sortAscDec xs = (insertSortAsc xs, insertSortDec xs)"
  ]
sortAscDecRefinement = "{-@ sortAscDec    :: (Ord a) => xs:[a] -> (IncrList a, DecrList a) @-}"
sortAscDecPiece = ProgramPiece "sortAscDec" sortAscDecImpl sortAscDecRefinement

insertionSortPieces = cycle [lteConditionPieceX 1, gteConditionPieceX 1, eqConditionPieceX 1, lteConditionPieceX 2, gteConditionPieceX 2, eqConditionPieceX 2, insertSortAscPiece, insertAscPiece, insertSortDecPiece, insertDecPiece, sortAscDecPiece, neqConditionPieceX 1, nullPiece]

andImpl = unlines
  [
    "boolOp :: Bool -> Bool -> Bool",
    "boolOp x y = x && y"
  ]
andRefinement = "{-@ boolOp :: x:Bool -> y:Bool -> {v:Bool | (v <=> x && y)} @-}"
andPiece = ProgramPiece "and" andImpl andRefinement

orImpl = unlines
  [
    "boolOp :: Bool -> Bool -> Bool",
    "boolOp x y = x || y"
  ]
orRefinement = "{-@ boolOp :: x:Bool -> y:Bool -> {v:Bool | (v <=> x || y)} @-}"
orPiece = ProgramPiece "or" orImpl orRefinement

insert2Impl = unlines [
  "insert :: (Ord a) => a -> [a] -> [a]",
  "insert y []     = [y]",
  "insert y (x:xs)",
  " | (boolOp (condition1 y x) (condition2 y x))      = y : x : xs",
  " | otherwise   = x : insert y xs"
  ]
insert2Refinement = "{-@ insert :: (Ord a) => a -> IncrList a -> IncrList a @-}"
insert2Piece = ProgramPiece "insert" insert2Impl insert2Refinement

insertSort2Impl = unlines [
  "insertSort :: (Ord a) => [a] -> [a]",
  "insertSort [] = []",
  "insertSort (x:xs) = insert x (insertSort xs)"
  ]
insertSort2Refinement = unlines [
  "{-@ type IncrList a = [a]<{\\xi xj -> xi <= xj}> @-}",
  "{-@ insertSort :: (Ord a) => xs:[a] -> (IncrList a) @-}"
  ]
insertSort2Piece = ProgramPiece "insertSort" insertSort2Impl insertSort2Refinement

insertionSort2Pieces = cycle [lteConditionPieceX 1, gteConditionPieceX 1, eqConditionPieceX 1, lteConditionPieceX 2, gteConditionPieceX 2, eqConditionPieceX 2, andPiece, orPiece, insert2Piece, insertSort2Piece, nullPiece]

-- pieces for quicksort
ltConditionImplX x = unlines 
  [
    "condition" ++ show x ++ ":: (Ord a) => a -> a -> Bool",
    "condition" ++ show x ++ " x y = x < y"
  ]
ltConditionRefinementX x = "{-@ condition" ++ show x ++ " :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x < y)} @-}"
ltConditionPieceX x = ProgramPiece ("lt" ++ show x) (ltConditionImplX x) (ltConditionRefinementX x)

filterLtImpl = unlines
  [
    "filterLt :: (Ord a) => a -> [a] -> [a]",
    "filterLt x xs = [y | y <- xs, condition1 y x]"
  ]
filterLtRefinement = "{-@ filterLt :: (Ord a) => x:a -> y:[a] -> {vv: [{v:a | v < x}] | len vv <= len y} @-}"
filterLtPiece = ProgramPiece "filterLt" filterLtImpl filterLtRefinement

filterGteImpl = unlines
  [
    "filterGte :: (Ord a) => a -> [a] -> [a]",
    "filterGte x xs = [y | y <- xs, condition2 y x]"
  ]
filterGteRefinement = "{-@ filterGte :: (Ord a) => x:a -> y:[a] -> {vv: [{v:a | v >= x}] | len vv <= len y} @-}"
filterGtePiece = ProgramPiece "filterGte" filterGteImpl filterGteRefinement

pivImpl = unlines
  [
    "pivApp :: (Ord a) => a -> [a] -> [a] -> [a]",
    "pivApp piv []     ys  = piv : ys",
    "pivApp piv (x:xs) ys  = x   : pivApp piv xs ys"
  ]
pivRefinement = unlines
  [
    "{-@ pivApp :: piv:a",
    "            -> IncrList {v:a | v <  piv}",
    "            -> IncrList {v:a | v >= piv}",
    "            -> IncrList a",
    "@-}"
  ]
pivPiece = ProgramPiece "piv" pivImpl pivRefinement

quickSortImpl = unlines
  [
    "quickSort :: (Ord a) => [a] -> [a]",
    "quickSort []     = []",
    "quickSort (x:xs) = pivApp x lts gts",
    "   where",
    "    lts          = quickSort (filterLt x xs)",
    "    gts          = quickSort (filterGte x xs)"
  ]
quickSortRefinement = unlines
  [
    "{-@ type IncrList a = [a]<{\\xi xj -> xi <= xj}> @-}",
    "{-@ quickSort    :: (Ord a) => [a] -> IncrList a @-}"
  ]
quickSortPiece = ProgramPiece "quickSort" quickSortImpl quickSortRefinement

quickSortPieces = cycle [ltConditionPieceX 1, gteConditionPieceX 1, ltConditionPieceX 2, gteConditionPieceX 2, filterLtPiece, filterGtePiece, pivPiece, quickSortPiece, eqConditionPieceX 1, eqConditionPieceX 2, nullPiece]

{- input/output examples for calculating fitness -}
type Input = [Int]

type Output = [Int]
-- type Output = ([Int], [Int])

filterEvensTests =
  [ ([0, 1, 2, 3, 4], [0, 2, 4]),
    ([1, 3, 5, 7], []),
    ([], []),
    ([3, 2, 4, 1, 5, 9], [2, 4])
  ]

boundTests =
  [ ([0, 1, 2, 3, 4, 5], map (max flags_chromosome_range) [0, 1, 2, 3, 4, 5]),
    ([5, 4, 3, 2, 1, 0], map (max flags_chromosome_range) [5, 4, 3, 2, 1, 0]),
    ([], [])
  ]

multiFilter2Tests =
  [
    ([], ([], [])),
    ([0, 1, 2, 3, 4], ([0, 2, 4], [1, 3])),
    ([4, 3, 2, 1, 0], ([4, 2, 0], [3, 1])),
    ([-3, 0, 2, -1, 1], ([0, 2], [-3, -1, 1])),
    ([3, 2, 4, 1, 9], ([2, 4], [3, 1, 9]))
  ]

insertionSortTests =
  [
    ([], ([], [])),
    ([1], ([1], [1])),
    ([0, 2, 1], ([0, 1, 2], [2, 1, 0])),
    ([3, 2, 1], ([1, 2, 3], [3, 2, 1])),
    ([4, 1, 9, -1], ([-1, 1, 4, 9], [9, 4, 1, -1]))
  ]

insertionSort2Tests =
  [
    ([], []),
    ([1], [1]),
    ([0, 2, 1], [0, 1, 2]),
    ([3, 2, 1], [1, 2, 3]),
    ([4, 1, 9, -1], [-1, 1, 4, 9])
  ]

quickSortTests = 
  [
    ([], []),
    ([1], [1]),
    ([2, 2], [2, 2]),
    ([0, 2, 1], [0, 1, 2]),
    ([3, 2, 1], [1, 2, 3]),
    ([4, 1, 9, -1], [-1, 1, 4, 9])
  ]


{- Program pieces and io examples, based on value of the "problem" flag -}
pieces = case flags_problem of
  FilterEvens -> filterEvensPieces
  Bound -> boundPieces
  MultiFilter2 -> multiFilter2Pieces
  MultiFilter3 -> multiFilter3Pieces
  InsertionSort -> insertionSortPieces
  InsertionSort2 -> insertionSort2Pieces
  QuickSort -> quickSortPieces

(test_inputs, expected_outputs) = case flags_problem of
  -- FilterEvens -> unzip filterEvensTests
  -- Bound -> unzip boundTests
  -- TODO multi filter tests have to be handled differently
  -- MultiFilter2 -> unzip multiFilter2Tests
  -- MultiFilter3 -> ([], [])
  -- InsertionSort -> unzip insertionSortTests
  InsertionSort2 -> unzip insertionSort2Tests
  QuickSort -> unzip quickSortTests

fnName = case flags_problem of
  FilterEvens -> "filterEvens"
  Bound -> "bound"
  MultiFilter2 -> "multiFilter"
  MultiFilter3 -> "multiFilter"
  InsertionSort -> "sortAscDec"
  InsertionSort2 -> "insertSort"
  QuickSort -> "quickSort"

{- options for writing to file -}
openFileFlags = OpenFileFlags {append = False, exclusive = False, noctty = False, nonBlock = False, trunc = True}

type Genotype = [Int]
type Fitness = Float
data GAIndividual = GAIndividual {genotype :: Genotype, fitness :: Fitness} deriving (Show, Eq)

{- Type for population-}
type Population = [GAIndividual]

{- Cache for fitness calculations -}
fitnessMemo :: Map Genotype Fitness
fitnessMemo = Data.Map.empty

{- Calls mutate on the population. Resets the individual since a
 change should occur. TODO (Could be smarter an verify if a reset is needed)-}
mutateOp :: Population -> [Float] -> [Int] -> Population
mutateOp [] _ _ = []
mutateOp (ind : pop) rndDs rndIs = repair (createIndiv (mutate'' (genotype ind) (take flags_chromosome_size rndDs) (take flags_chromosome_size rndIs))) (drop flags_chromosome_size rndIs) : mutateOp pop (drop flags_chromosome_size rndDs) (drop 100 rndIs)

{- Mutate a genotype by uniformly changing the integer. -}
mutate'' :: Genotype -> [Float] -> [Int] -> [Int]
mutate'' [] _ _ = []
mutate'' _ [] _ = []
mutate'' _ _ [] = []
mutate'' (c : cs) (rndD : rndDs) (rndI : rndIs) = (if rndD > mutationRate then c else rndI) : mutate'' cs rndDs rndIs

{- Calls crossover on the population TODO How does it handle oddnumber
 sized populations? Fold? Smarter resetting values in individual TODO hardcoding rnd drop-}
xoverOp :: Population -> [Float] -> [Int] -> Population
xoverOp [] _ _ = []
xoverOp (ind1 : ind2 : pop) rndDs rndIs =
  let (child1, child2) = xover (genotype ind1, genotype ind2) (take 2 rndDs)
   in repair (createIndiv child1) rndIs : repair (createIndiv child2) (drop 10 rndIs) : xoverOp pop (drop 2 rndDs) (drop 100 rndIs)
xoverOp (ind1 : []) rndDs rndIs = [ind1]

{- Singlepoint crossover, crossover probability is hardcoded-}
xover :: (Genotype, Genotype) -> [Float] -> (Genotype, Genotype)
xover ([], _) _ = ([], [])
xover (_, []) _ = ([], [])
xover (_, _) [] = error "Empty rnd"
xover (p1, p2) (rndD : rndDs) =
  if rndD < crossoverRate
    then -- Remove the used random values for the rndDs for the xopoints calls

      let xopoint1 = xopoint rndDs p1
       in (take xopoint1 p1 ++ drop xopoint1 p2, take xopoint1 p2 ++ drop xopoint1 p1)
    else (p1, p2)

{- Utility function for getting crossover point TODO Make nicerway of returning 1 as a minimum value -}
xopoint :: [Float] -> Genotype -> Int
xopoint [] _ = error "Empty rnd"
xopoint _ [] = error "Empty genotype"
xopoint (rnd : rndDs) codons = max 1 (round $ (rnd) * (fromIntegral $ length codons))

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
selectIndividuals (rnd : rndIs) pop tournamentSize = (pop !! (rnd `mod` (length pop))) : selectIndividuals rndIs pop (tournamentSize - 1)

{- Generational replacement with elites. TODO error catching-}
generationalReplacementOp :: Population -> Population -> Int -> Population
generationalReplacementOp orgPop newPop elites =
  let pop = (take elites $ sortBy sortInd orgPop) ++ (take (length newPop - elites) $ sortBy sortInd newPop)
   in --trace ("\n\n" ++ showPop orgPop ++ "\n" ++ showPop newPop ++ "\n" ++ showPop pop ++ "\n")
      trace (showPop pop)
      pop

showInd :: GAIndividual -> String
showInd (GAIndividual genotype fitness) = "Fit:" ++ show fitness ++ ", Genotype: " ++ show genotype

showPop :: Population -> String
showPop [] = ""
showPop (ind : pop) = showInd ind ++ "\n" ++ showPop pop

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
  synthFile <- openFd fname WriteOnly (Just ownerModes) openFileFlags
  fdWrite synthFile s
  closeFd synthFile

{- Add a main function so refinement type check catches when something is not defined -}
filterEvensMain =
  unlines
    [ "test = [1, 3, 4, 6, 7, 2]",
      "main = do",
      "        putStrLn $ \"original: \" ++ show test",
      "        putStrLn $ \"evens: \" ++ show (filterEvens test)"
    ]

boundMain =
  unlines
    [ "test = [0, 1, 2, 3, 4, 5, 6, 7]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         putStrLn $ \"truncated: \" ++ show (bound test)",
      "         putStrLn $ \"truncated: \" ++ show (bound test)",
      "         putStrLn $ \"truncated: \" ++ show (bound test)",
      "         putStrLn $ \"truncated: \" ++ show (bound test)", 
      "         putStrLn $ \"truncated: \" ++ show (bound test)",
      "         putStrLn $ \"truncated: \" ++ show (bound test)", 
      "         putStrLn $ \"truncated: \" ++ show (bound test)",
      "         putStrLn $ \"truncated: \" ++ show (bound test)", 
      "         putStrLn $ \"truncated: \" ++ show (bound test)"
    ]

multiFilter2Main =
  unlines
    [ "test = [1, 3, 4, 6, 7, 2]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         let (evens, odds) = multiFilter test",
      "         putStrLn $ \"evens: \" ++ show evens",
      "         putStrLn $ \"odds: \" ++ show odds"
    ]

multiFilter3Main =
  unlines
    [ "test = [1, 3, 4, 6, 7, 2]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         let (evens, odds, gt2) = multiFilter test",
      "         putStrLn $ \"evens: \" ++ show evens",
      "         putStrLn $ \"odds: \" ++ show odds",
      "         putStrLn $ \"gt2: \" ++ show gt2"
    ]

insertionSortMain = 
  unlines
    [
      "test = [3, 1, 4, 6, 7, 2]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         putStrLn $ \"sorted: \" ++ show (sortAscDec test)"
    ]

insertionSort2Main = 
  unlines
    [
      "test = [3, 1, 4, 6, 7, 2]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         putStrLn $ \"sorted: \" ++ show (insertSort test)"
    ]

quickSortMain = 
  unlines
    [
      "test = [3, 1, 4, 6, 7, 2]",
      "main = do",
      "         putStrLn $ \"original: \" ++ show test",
      "         putStrLn $ \"sorted: \" ++ show (quickSort test)"
    ]

mainPiece = case flags_problem of
  FilterEvens -> filterEvensMain
  Bound -> boundMain
  MultiFilter2 -> multiFilter2Main
  MultiFilter3 -> multiFilter3Main
  InsertionSort -> insertionSortMain
  InsertionSort2 -> insertionSort2Main
  QuickSort -> quickSortMain

{- Use refinement type checking to calculate fitness. -}
{-# NOINLINE refinementTypeCheck #-}
refinementTypeCheck :: Genotype -> Fitness
refinementTypeCheck g = unsafePerformIO $ do
  let s = combinePiecesWithRefinement (map (pieces !!) g) ++ mainPiece
  do
    writeToFilePosix flags_fname s
    cfg <- getOpts [flags_fname]
    (ec, _) <- runLiquid Nothing cfg
    if ec == ExitSuccess then return 1 else return 0

data RefinementErrInfo = Invalid Int | Unsafe Int deriving (Eq)

{- New refinement type check where we inspect stdout -}
refinementTypeCheckNew :: Genotype -> Fitness
refinementTypeCheckNew g = unsafePerformIO $ do
  let s = combinePiecesWithRefinement (map (pieces !!) g) ++ mainPiece
  do
    writeToFilePosix flags_fname s
    (ec, stdout, _) <- readProcessWithExitCode "liquid" [flags_fname] ""
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
  | not (null (indices (pack "LIQUID: ERROR") text)) = Invalid count
  | otherwise = error ("unexpected error output from liquid: " ++ s)
  where
    text = pack s
    count = length (indices (pack "error: ") text)

{- Check input/output examples by writing to file then using eval -}
{-# NOINLINE evalIOExamples #-}
evalIOExamples :: Genotype -> Fitness
evalIOExamples g = unsafePerformIO $ do
  let s = combinePieces (map (pieces !!) g)
  writeToFilePosix flags_fname s
  r <- runInterpreter $ do
    loadModules [flags_fname]
    setImports ["Prelude"]
    modules <- getLoadedModules
    setTopLevelModules modules
    interpret fnName (as :: (Input -> Output))
  case r of
    Left err -> return 0
    Right f -> return $ fromIntegral (checkIOExamples (map f test_inputs) expected_outputs) / fromIntegral (length expected_outputs)

{- Calculate the number of examples that were correct -}
checkIOExamples :: [Output] -> [Output] -> Int
checkIOExamples [] [] = 0
checkIOExamples (x : xs) (y : ys)
  -- first list is the actual output, second list is expected output
  | x == y = 1 + checkIOExamples xs ys
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
            RefinementTypesNew -> refinementTypeCheckNew g
            IOExamples -> evalIOExamples g
      let newState = Data.Map.insert g fitness memo
      put newState
      newMemo <- Control.Monad.State.get
      return fitness
    Just fitness -> return fitness

{- Calculate fitness on a population -}
calculateFitnessOp :: Population -> State (Map Genotype Fitness) Population
calculateFitnessOp [] = return []
calculateFitnessOp (ind : pop) = do
  memo <- Control.Monad.State.get
  let (fitness, memo2) = runState (calculateFitness (genotype ind)) memo
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
createPop popCnt rndInts = repair (createIndiv (take flags_chromosome_size rndInts)) (drop flags_chromosome_size rndInts) : createPop (popCnt -1) (drop 100 rndInts)

{- "Repair" a chromosome - use rndInts to remove duplicates if flags_disallow_dups is set -}
repair :: GAIndividual -> [Int] -> GAIndividual
repair ind rndInts = if not flags_disallow_dups || (length uniqs == length (genotype ind)) then ind
                      else repair (GAIndividual newGeno fit) (drop n rndInts)
                        where
                          uniqs = nub (genotype ind)
                          n = flags_chromosome_size - length uniqs
                          newGeno = uniqs ++ take n rndInts
                          fit = evalState (calculateFitness newGeno) fitnessMemo

{- Evolve the population recursively counting with genotype and
returning a population of the best individuals of each
generation. Hard coding tournament size and elite size TODO drop a less arbitrary value of random values than 10-}
evolve :: Population -> [Int] -> [Int] -> Int -> [Float] -> State (Map Genotype Fitness) Population
evolve pop _ _ 0 _ = return []
evolve [] _ _ _ _ = return (error "Empty population")
evolve pop rndVs rndIs gen rndDs = do
  memo <- Control.Monad.State.get
  let (newPop, memo2) = runState (calculateFitnessOp (mutateOp (xoverOp (tournamentSelectionOp (length pop) pop rndIs tournamentSize) rndDs rndVs) rndDs rndVs)) memo
  let (rest, memo3) = runState (evolve (generationalReplacementOp pop newPop eliteSize) (drop (flags_pop_size * 10) rndVs) (drop (flags_pop_size * 10) rndIs) (gen - 1) (drop (flags_pop_size * 10) rndDs)) memo2
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
bestInd (ind : pop) best = foldr best ind pop

{- Generates random numbers in range (0, n). -}
randoms' :: (RandomGen g, Random a, Num a) => a -> g -> [a]
randoms' n gen = let (value, newGen) = randomR (0, n) gen in value : randoms' n newGen

{- Run n trials and return list of ints representing how many
 - generations it took to find the optimal solution -}
runTrials :: RandomGen g => Int -> g -> State (Map Genotype Fitness) [Maybe Float]
runTrials 0 _ = return []
runTrials n gen = do
  memo <- Control.Monad.State.get
  let (g1, g2) = split gen
  let randV = randoms' flags_chromosome_range g1 :: [Int]
  let randI = randoms' flags_pop_size g1 :: [Int]
  let randNumberD = randoms' 1.0 g1 :: [Float]
  let pop_init = createPop flags_pop_size randV
  -- same pop, just calculate fitness on all of the individuals
  let (pop, memo2) = runState (calculateFitnessOp pop_init) memo
  let (bestInds, memo3) =
        trace (showPop pop) 
        (runState (evolve pop randV randI flags_generations randNumberD) memo2)
  let val = case flags_eval of
        GensToOptimal -> fmap fromIntegral $ findIndex (\x -> fitness x == 1.0) bestInds
        BestFitness -> Just $ fitness $ bestInd bestInds maxInd
  let (rest, memo4) = runState (runTrials (n -1) g2) memo3
  put memo4
  return (trace (showPop bestInds) (val : rest))

{- Random search: generate pop size * generations random individuals
 - and return index / 10 of the first individual that is optimal -}
runTrialsRandomSearch :: RandomGen g => Int -> g -> State (Map Genotype Fitness) [Maybe Float]
runTrialsRandomSearch 0 _ = return []
runTrialsRandomSearch n gen = do
  memo <- Control.Monad.State.get
  let (g1, g2) = split gen
  let randNumber = randoms' flags_chromosome_range g1 :: [Int]
  let pop = createPop (flags_pop_size * flags_generations) randNumber
  let (inds, memo2) = runState (calculateFitnessOp pop) memo
  let val = case flags_eval of
        GensToOptimal -> fmap fromIntegral $ fmap ((+ 1) . (`div` flags_pop_size)) (findIndex (\x -> fitness x == 1.0) inds)
        BestFitness -> Just $ fitness $ bestInd inds maxInd
  let (rest, memo3) = 
        trace (showPop inds)
        (runState (runTrialsRandomSearch (n -1) g2) memo2)
  put memo3
  return (val : rest)

{- Print all the summary stats given result list -}
printSummary :: [Maybe Float] -> IO ()
printSummary vals = do
  let failed = length $ filter isNothing vals
  let reals = Data.Vector.fromList $ map (float2Double . fromMaybe (fromIntegral flags_generations)) vals
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

{- Takes a list of genotypes and prints fitness for each one -}
fitnessAll :: [[Int]] -> IO ()
fitnessAll [] = return ()
fitnessAll (x : xs) = do
  putStrLn $ show x ++ ": " ++ show (evalState (calculateFitness x) fitnessMemo)
  fitnessAll xs

{- Run the GA -}
main = do
  _ <- $initHFlags ""
  gen <- getStdGen
  print $ evalState (calculateFitness [0, 4, 6, 7, 8, 9, 10]) fitnessMemo
  if flags_fitness_all
    then fitnessAll $ replicateM flags_chromosome_size [0..flags_chromosome_range]
    else do
      let vals =
            if flags_random_search
              then evalState (runTrialsRandomSearch flags_num_trials gen) fitnessMemo
              else evalState (runTrials flags_num_trials gen) fitnessMemo
      printSummary vals

