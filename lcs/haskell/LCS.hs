-- http://en.wikipedia.org/wiki/Longest_common_substring_problem
-- uses sparse arrays

import Control.Lens.Indexed
import qualified Data.Foldable as F
import Control.Monad

import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import Text.Printf

type MyIndex = (Int,Int)

main = do
    name <- getProgName
    args <- getArgs
    when (length args /= 2) $ do
        printf "Usage: %s <string_1> <string_2>\n" name
        exitSuccess
    let [s1, s2] = args
    printf "The longest common substrings are:\n"
    mapM_ (printf "\t%s\n") (findSubstrings s1 (findAllCommonSubstrings s1 s2))


findAllCommonSubstrings :: String -> String -> H.HashMap MyIndex Int
-- fold over s1
findAllCommonSubstrings s1 s2 = ifoldl overS2 H.empty s1
    where
    -- fold over s2
    overS2 i memo c = ifoldr ifEqUpdateTable memo s2
        where
        ifEqUpdateTable j c'
            | c == c'   = H.insert (i,j) (H.lookupDefault 0 oldK memo + 1) . H.delete oldK
            | otherwise = id
            where
            oldK = (i-1, j-1)

findSubstrings :: String -> H.HashMap MyIndex Int -> [String]
findSubstrings s substrings = map (\((ix,_),n) -> V.toList (V.slice (ix-n+1) n sVec)) highest
    where
    sVec = V.fromList s
    -- could keep track of the highest rather than go through it each time
    highest = H.toList (H.filter (== F.maximum substrings) substrings)
