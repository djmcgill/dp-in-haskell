module NaiveParse where

import Control.DeepSeq

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- readFile filename
    let ([cap, _]:xss) = map (map read . words) (lines file)
    let vws = map getVW xss
    rnf vws `seq` return (cap, vws)

    where
    getVW :: [Int] -> (Int,Int)
    getVW [v, w] = (v,w)