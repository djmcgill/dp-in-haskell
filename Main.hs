module Main where

import Knapsack

{-
gcd = 1
selection = fromList [(6525,40),(9967,30)]
total value = 69360
total weight = 7040
-}

main = do
    (cap, vws) <- readProblem "test_problem_1.data"
    print $ solve vws cap

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- readFile filename
    let ([cap, _]:xss) = map (map read . words) (lines file)
    return (cap, map getVW xss)

    where
    getVW :: [Int] -> (Int,Int)
    getVW [v, w] | v >= 0 && w > 0 = (v,w)
                 | otherwise = error "assertion 'v >= 0 && w > 0' failed in getVW"