{-@ condition1 :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}
condition1 :: Int -> Bool
condition1 x = x `mod` 2 == 0

{-@ condition2 :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}
condition2 :: Int -> Bool
condition2 x = x `mod` 2 /= 0

{-@ type Even = {v:Int | v mod 2 = 0} @-}
{-@ type Odd = {v:Int | v mod 2 /= 0} @-}
{-@ filterEvenOdd :: [Int] -> ([Even], [Odd]) @-}
filterEvenOdd :: [Int] -> ([Int], [Int])
filterEvenOdd xs = ([a | a <- xs, condition1 a], [a | a <- xs, condition2 a])

test = [1, 3, 4, 6, 7, 2]
main = do
    putStrLn $ "original: " ++ show test
    let (evens, odds) = filterEvenOdd test
    putStrLn $ "evens: " ++ show evens
    putStrLn $ "evens: " ++ show odds