{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Knapsack (solve) where

import           Control.Arrow       (second, (***))
import qualified Data.IntMap.Strict as I
import           Data.List           (foldl')
import           Data.Maybe          (catMaybes)
import           Data.Ord            (comparing)
import           System.Random
import qualified Data.Vector as V

{- TODO:
talk about
    lazy vs strict in Solution
    making safeMaximum strict basically eliminiated all the Solutions on the stack
use criteron to compare the lazy vs strict versions on the SAME problem

ALLOCATION:
    Int
    Integer
    (,)
    (*)

COST CENTRE                  MODULE  %time %alloc

knapsackScaled.genI.eachPair Main     45.9   30.1
knapsackScaled.genI          Main     38.3   67.9
compare                      Main      6.7    0.0
safeMaximum                  Main      5.3    0.0
egRandom                     Main      3.0    1.4
-}

type Selection = I.IntMap Int
newtype Value = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

data Solution = Solution {selection :: !Selection, totalValue :: !Value, totalWeight :: !Weight}
    deriving (Eq, Show)

emptySoln = Solution I.empty 0 0

instance Ord Solution where
    compare = comparing totalValue
{-
main = print =<< egRandom (10000 :: Int)

egRandom n = do
    (cap:xs) <- randomRs (1,1000 :: Int) `fmap` newStdGen
    let vws = take n $ pair xs
    return (solve vws cap)
    where
    pair :: [a] -> [(a,a)]
    pair (x1:x2:xs) = (x1,x2) : pair xs
    pair _ = []
-}
-- | Given a list of (Value, Weight) pairs, maximises the total value
--   while keeping the total weight under the capacity.
solve :: [(Int, Int)] -> Int -> (I.IntMap Int, Int, Int)
solve vws cap = (s, v, w)
    where Solution s (V v) (W w) = knapsackNative (V.fromList $ map (V *** W) vws) (W cap)

knapsackNative :: V.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (V.map (second scale) vws) (scale cap)
    where
    -- the gcd of the weights
    gcdW = V.foldl1' gcd $ V.map snd vws
    scale x = div x gcdW
    unscale (Solution s v w) = Solution s v (w*gcdW)

knapsackScaled :: V.Vector (Value, Weight) -> Weight -> Solution
knapsackScaled vws (W cap) = solns V.! cap
    where
    -- a vector of the best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = V.generate (cap+1) genI

    genI :: Int -> Solution
    genI 0 = emptySoln
    genI !i = safeMaximum $ catMaybes $ V.toList $ V.imap eachPair vws
        where
        eachPair :: Int -> (Value, Weight) -> Maybe Solution
        eachPair !j (v, w) | unW w <= i = case solns V.! (i - unW w) of
            Solution s' v' w' -> Just $ Solution (I.insertWith (+) j 1 s') (v' + v) (w' + w)
        eachPair _ _ = Nothing

safeMaximum :: [Solution] -> Solution
safeMaximum = foldl' max emptySoln