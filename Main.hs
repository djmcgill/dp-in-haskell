module Main where

import Knapsack

main = do
    (cap, vws) <- readProblem "test_problem_1.data"
    print $ solve vws cap

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- readFile filename
    let (capStr:xsStr) = lines file
    return (read capStr, map readVW xsStr)

    where
    readVW :: String -> (Int,Int)
    readVW str = (read vStr, read wStr)
        where [vStr, wStr] = words str