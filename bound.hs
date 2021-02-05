{- If number < 2, set = 2 -}
{-@ boundLower :: x:Int -> {v:Int | v >= 2 } @-}
boundLower :: Int -> Int
boundLower = max 2

{- Bound all the numbers in the list so 2 <= x -}
{-@ type GreaterThanTwo = {v:Int | 2 <= v} @-}
{-@ bound :: xs: [Int] -> [GreaterThanTwo] / [len xs] @-}
bound :: [Int] -> [Int]
bound = map boundLower

test = [0, 1, 2, 3, 4, 5, 6, 7]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "truncated: " ++ show (bound test)