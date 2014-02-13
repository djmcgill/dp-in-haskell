module AttoparsecDecimalParse where

import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy as B

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- B.readFile filename
    result@(_,vws) <- either error return $ eitherResult $ flip parse file $ do
        cap <- decimal
        skipWhile (not . isEndOfLine)
        endOfLine
        vws <- many $ ((,) <$> (decimal <* space) <*> decimal <* endOfLine)
        return (cap, vws)
    rnf vws `seq` return result


