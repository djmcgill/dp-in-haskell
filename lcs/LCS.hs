-- http://en.wikipedia.org/wiki/Longest_common_substring_problem
-- uses sparse arrays

import Control.Lens
import Control.Lens.Indexed (ifoldr)
import qualified Data.Foldable as F
import Data.List (foldl', sort)
import Control.Monad

import qualified Data.HashMap.Lazy as H
import Data.Word
import qualified Data.Vector as V

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Text.Printf

import Debug.Trace

type MyIndex = (Int,Int)

{- main = do
    let [s1, s2] = ["sdfaacrtgdv", "ergdfaacecsgw"]
    printf "The Longest common substrings are:\n"
    mapM_ (printf "\t%s\n") (findSubstrings s1 (lcs s1 s2)) -}

main = do
    name <- getProgName
    args <- getArgs
    when (length args /= 2) $ do
        printf "Usage: %s <string_1> <string_2>\n" name
        exitSuccess
    let [s1, s2] = args
    printf "The longest common substrings are:\n"
    mapM_ (printf "\t%s\n") (findSubstrings s1 (lcs s1 s2))


lcs :: String -> String -> H.HashMap MyIndex Int
-- fold over s1
lcs s1 s2 = ifoldl updateMemo H.empty s1
    where
    -- fold over s2
    updateMemo i memo c = ifoldr (\ix c' -> if c == c' then updateAtIx ix else id) memo s2
        where
        updateAtIx j = H.insert (i,j) (H.lookupDefault 0 oldK memo + 1) . H.delete oldK
            where
            oldK = (i-1, j-1)

findSubstrings :: String -> H.HashMap MyIndex Int -> [String]
findSubstrings s indices = map (\((ix,_),n) -> V.toList (V.slice (ix-n+1) n sVec)) highest
    where
    sVec = V.fromList s
    -- could keep track of the highest rather than go through it each time
    highest :: [(MyIndex, Int)]
    highest = H.toList (H.filter (== F.maximum indices) indices)
