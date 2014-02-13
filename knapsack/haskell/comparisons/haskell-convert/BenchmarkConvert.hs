module Main where

import           Control.Applicative
import           Control.Monad (forM_)
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy as B
import           Data.IntMap (toList)
import           Text.Printf

import qualified KnapsackGenerate as G
import qualified KnapsackConvert as C

import Criterion.Config
import Criterion.Main

main = do
    (cap, vws) <- readProblem "test_problem_1.data"
    defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
            bench "knapsack-generate" $ nf (G.solve vws) cap
          , bench "knapsack-convert" $ nf (C.solve vws) cap
        ]]

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        cap <- decimal
        skipWhile (not . isEndOfLine)
        endOfLine
        vws <- many $ ((,) <$> (decimal <* space) <*> decimal <* endOfLine)
        return (cap, vws)













