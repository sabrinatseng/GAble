{-@ condition1 :: x:Int -> {v:Bool | (v <=> (x mod 2 == 0))} @-}
condition1 :: Int -> Bool
condition1 x = x `mod` 2 == 0

{-@ condition2 :: x:Int -> {v:Bool | (v <=> (x mod 2 /= 0))} @-}
condition2 :: Int -> Bool
condition2 x = x `mod` 2 /= 0

{-@ condition3 :: x:Int -> {v:Bool | (v <=> (x > 2))} @-}
condition3 :: Int -> Bool
condition3 x = x > 2

{-@ type Even = {v:Int | v mod 2 = 0} @-}
{-@ type Odd = {v:Int | v mod 2 /= 0} @-}
{-@ type GT2 = {v:Int | v > 2} @-}
{-@ multiFilter :: [Int] -> ([Even], [Odd], [GT2]) @-}
multiFilter :: [Int] -> ([Int], [Int], [Int])
multiFilter xs = ([a | a <- xs, condition1 a], [a | a <- xs, condition2 a], [a | a <- xs, condition3 a])

test = [1, 3, 4, 6, 7, 2]
main = do
    putStrLn $ "original: " ++ show test
    let (evens, odds, gt2) = multiFilter test
    putStrLn $ "evens: " ++ show evens
    putStrLn $ "evens: " ++ show odds
    putStrLn $ ">2: " ++ show gt2