{-@ condition :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}
condition :: Int -> Bool
condition x = x `mod` 2 == 0

{-@ type Even = {v:Int | v mod 2 = 0} @-}
{-@ filterEvens :: [Int] -> [Even] @-}
filterEvens :: [Int] -> [Int]
filterEvens xs = [a | a <- xs, condition a]

test = [1, 3, 4, 6, 7, 2]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "evens: " ++ show (filterEvens test)