{-@ boolOp :: x:Bool -> y:Bool -> {v:Bool | (v <=> x || y)} @-}
boolOp :: Bool -> Bool -> Bool
boolOp x y = x || y

{-@ condition1 :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x <= y)} @-}
condition1 :: (Ord a) => a -> a -> Bool
condition1 x y = x <= y

{-@ condition2 :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}
condition2 :: (Ord a) => a -> a -> Bool
condition2 x y = x == y

-- {-@ condition :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}
-- condition x y = x == y

-- {-@ condition :: Ord a => x:a -> y:a -> {v:Bool | (v <=> x == y)} @-}
-- condition x y = x != y

{-@ type IncrList a = [a]<{\xi xj -> xi <= xj}> @-}

{-@ insert :: (Ord a) => a -> IncrList a -> IncrList a @-}
insert :: (Ord a) => a -> [a] -> [a]
insert y []     = [y]
insert y (x:xs) 
   | (boolOp (condition1 y x) (condition2 y x))      = y : x : xs 
   | otherwise   = x : insert y xs

{-@ insertSort    :: (Ord a) => xs:[a] -> (IncrList a) @-}
insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

test = [3, 1, 4, 6, 7, 2]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "sorted: " ++ show (insertSort test)