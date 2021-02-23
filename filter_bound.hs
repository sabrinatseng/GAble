{- If number < 2, set = 2 -}
{-@ boundLower :: x:Int -> {v:Int | v >= 2 } @-}
boundLower :: Int -> Int
boundLower = max 2

{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}
condition :: Int -> Bool
condition x = x `mod` 2 == 0

{-@ type Even = {v:Int | v mod 2 = 0} @-}
{-@ filterEvens :: [Int] -> [Even] @-}
filterEvens :: [Int] -> [Int]
filterEvens xs = [a | a <- xs, condition a]

{-@ type EvenOverTwo = {v:Int | (v mod 2 = 0 && v >= 2)} @-}
{-@ filterBound :: [Int] -> [EvenOverTwo] @-}
filterBound :: [Int] -> [Int]
filterBound xs = [boundLower a | a <- xs, condition a]

test = [0, 1, 2, 3, 4, 5, 6, 7]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "truncated: " ++ show (filterBound test)