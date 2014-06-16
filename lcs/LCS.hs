-- http://en.wikipedia.org/wiki/Longest_common_substring_problem
-- uses sparse arrays

import Prelude hiding (foldr)

import Control.Monad

import qualified Data.HashMap.Lazy as H
import Data.Vector.Unboxed hiding (length, map, mapM_)
import Data.Word

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Text.Printf

type Index = Int -- #strings-1 ints
fromString :: String -> Vector Char
fromString = undefined

toString :: Vector Char -> String
toString = undefined

main = do
    name <- getProgName
    args <- getArgs
    when (length args /= 2) $ do
        printf "Usage: %s <string_1> <string_2>" name
        exitSuccess
    let [s1, s2] =  map fromString args
    printf "The Longest common substrings are\n:"
    mapM_ (printf "\t%s\n") (findSubstrings s1 (lcs s1 s2))


lcs :: Vector Char -> Vector Char -> H.HashMap Index Int
lcs s1 s2 = foldr updateMemo H.empty s1
    where
    updateMemo :: Char -> H.HashMap Index Int -> H.HashMap Index Int
    updateMemo c memo = ifoldr' (\ix c' -> if c == c' then updateAtIx ix else id) H.empty s2
        where updateAtIx ix = H.insert ix (H.lookupDefault 0 (ix-1) memo)

findSubstrings :: Vector Char -> H.HashMap Index Int -> [String]
findSubstrings s indices = map (\(ix,n) -> toString (slice (ix-n) n s)) highest
    where
    highest :: [(Index, Int)]
    highest = undefined
