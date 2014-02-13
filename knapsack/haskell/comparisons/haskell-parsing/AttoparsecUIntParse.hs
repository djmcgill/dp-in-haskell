module AttoparsecUIntParse where

import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.ByteString.Lazy
import Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lex.Integral

readProblem :: String -> IO (Int, [(Int,Int)])
readProblem filename = do
    file <- B.readFile filename
    result@(_, vws) <- either error return $ eitherResult $ flip parse file $ do
        cap <- uint
        skipWhile (not . isEndOfLine)
        endOfLine
        vws <- many $ ((,) <$> (uint <* space) <*> uint <* endOfLine)
        return (cap, vws)
    rnf result `seq` return result

    where
    uint :: Parser Int
    uint = readDecimal_ <$> takeWhile1 isDigit_w8


