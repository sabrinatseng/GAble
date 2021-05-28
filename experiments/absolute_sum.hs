{-@ condition :: x:Int -> {v:Bool | (v <=> x >= 0)} @-}
condition :: Int -> Bool
condition x = x >= 0

{-@ square :: x:Int -> {v:Int | v == x*x} @-}
square :: Int -> Int
square x = x*x

{-@ double :: x:Int -> {v:Int | v == x*2} @-}
double :: Int -> Int
double x = x*2

{-@ abz :: x:Int -> {v:Int | v >= 0} @-}
abz :: Int -> Int
abz x = if condition x then x else (-x)

{-@ abzSum :: x:[Int] -> {v:Int | v >= 0} @-}
abzSum :: [Int] -> Int
abzSum [] = 0
abzSum (x:xs) = abz x + abzSum xs

{-@ squareSum :: x:[Int] -> {v:Int | v >= 0} @-}
squareSum :: [Int] -> Int
squareSum [] = 0
squareSum (x:xs) = square x + squareSum xs

test = [-1, 1, 2, 0, -4]
main = do
    putStrLn $ "list: " ++ show test
    putStrLn $ "abs sum: " ++ show (abzSum test)