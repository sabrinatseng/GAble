{-@ condition1 :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x <= y)} @-}
condition1 :: (Ord a) => a -> a -> Bool
condition1 x y = x <= y

{-@ condition2 :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x >= y)} @-}
condition2 :: (Ord a) => a -> a -> Bool
condition2 x y = x >= y

-- {-@ condition :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}
-- condition x y = x == y

-- {-@ condition :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}
-- condition x y = x != y

{-@ type IncrList a = [a]<{\xi xj -> xi <= xj}> @-}
{-@ type DecrList a = [a]<{\xi xj -> xi >= xj}> @-}

{-@ insertAsc :: (Ord a) => a -> IncrList a -> IncrList a @-}
insertAsc :: (Ord a) => a -> [a] -> [a]
insertAsc y []     = [y]
insertAsc y (x:xs) 
   | (condition1 y x)      = y : x : xs 
   | otherwise   = x : insertAsc y xs

{-@ insertDec :: (Ord a) => a -> DecrList a -> DecrList a @-}
insertDec :: (Ord a) => a -> [a] -> [a]
insertDec y []     = [y]
insertDec y (x:xs) 
   | (condition2 y x)      = y : x : xs 
   | otherwise   = x : insertDec y xs

{-@ insertSortAsc    :: (Ord a) => xs:[a] -> (IncrList a) @-}
insertSortAsc :: (Ord a) => [a] -> [a]
insertSortAsc [] = []
insertSortAsc (x:xs) = insertAsc x (insertSortAsc xs)

{-@ insertSortDec    :: (Ord a) => xs:[a] -> (DecrList a) @-}
insertSortDec :: (Ord a) => [a] -> [a]
insertSortDec [] = []
insertSortDec (x:xs) = insertDec x (insertSortDec xs)

{-@ sortAscDec    :: (Ord a) => xs:[a] -> (IncrList a, DecrList a) @-}
sortAscDec :: (Ord a) => [a] -> ([a], [a])
sortAscDec xs = (insertSortAsc xs, insertSortDec xs)

test = [3, 1, 4, 6, 7, 2]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "sorted: " ++ show (sortAscDec test)