-- print a knapscack problem to a file. The first line is the capacity then all the other lines
-- are the value then weight seperated by a space.
import System.Environment
import System.Random

minR, maxR :: Int
minR = 100
maxR = 1000

main = maybe printUsage (uncurry writeLines) =<< parseArgs `fmap` getArgs

printUsage = putStrLn "Usage: <executable> <number of items> <filename>"

parseArgs :: [String] -> Maybe (Int, String)
parseArgs [nS, filename] = case reads nS of
    (n,""):_ -> Just (n, filename)
    _        -> Nothing
parseArgs _ = Nothing

writeLines n filename = do
    gen <- newStdGen
    let (cap, gen') = randomR (minR, maxR) gen
    let cap' = show (10*cap)
    let xs = map show (randomRs (minR, maxR) gen')
    writeFile filename $ unlines $ (cap' ++ " " ++ show n) : take n (pair xs)

    where
    pair :: [String] -> [String]
    pair (x1:x2:xs) = (x1 ++ " " ++ x2) : pair xs
    pair _ = []
