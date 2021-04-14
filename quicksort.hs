{-@ pivApp :: piv:a 
            -> IncrList {v:a | v <  piv} 
            -> IncrList {v:a | v >= piv} 
            -> IncrList a 
@-}
pivApp piv []     ys  = piv : ys
pivApp piv (x:xs) ys  = x   : pivApp piv xs ys

{-@ condition1 :: (Ord a) => x:a -> y:a -> {v:Bool | (v <=> x < y)} @-}
condition1 x y = x < y

{-@ condition2 :: (Ord a) => x:a -> y:a -> {v:Bool | (v <=> x >= y)} @-}
condition2 x y = x >= y

{-@ filterLt :: (Ord a) => x:a -> y:[a] -> {vv: [{v:a | v < x}] | len vv <= len y} @-}
filterLt x xs = [y | y <- xs, condition1 y x]

{-@ filterGte :: (Ord a) => x:a -> y:[a] -> {vv: [{v:a | v >= x}] | len vv <= len y} @-}
filterGte x xs = [z | z <- xs, condition2 z x]

{-@ type IncrList a = [a]<{\xi xj -> xi <= xj}> @-}
{-@ quickSort    :: (Ord a) => [a] -> IncrList a @-}
quickSort []     = []
quickSort (x:xs) = pivApp x lts gts
    where 
        lts          = quickSort (filterLt x xs)
        gts          = quickSort (filterGte x xs)

test = [3, 4, 1, 2, 9, 5]
main = do
    putStrLn $ "original: " ++ show test
    putStrLn $ "sorted: " ++ show (quickSort test)