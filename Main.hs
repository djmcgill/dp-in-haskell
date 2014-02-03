module Main where

import           Control.Applicative
import           Control.Monad (forM_)
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy as B
import           Data.IntMap (toList)
import           Text.Printf

import Knapsack

main = do
    (cap, vws) <- readProblem "test_problem_1.data"
    let (selection, v, w) = solve vws cap

    printf "\nThe Haskell solution is has a total weight of %i, a total value of %i and a selection of:\n" w v
    forM_ (toList selection) $ uncurry (printf "\tindex: %i, quantity: %i\n")

readProblem :: FileName -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        cap <- decimal
        skipWhile (not . isEndOfLine)
        endOfLine
        vws <- many $ ((,) <$> (decimal <* space) <*> decimal <* endOfLine)
        return (cap, vws)













