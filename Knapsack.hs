{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--module Knapsack where

import Control.Arrow (second, (***))
import Control.Applicative ((<$>))
import qualified Data.IntMap.Strict as I
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.Random
import qualified Data.Vector as V

{-
knapsackScaled.genI.eachPair Main     44.1   33.8
knapsackScaled.genI          Main     32.6   59.7
safeMaximum                  Main     10.4    5.2
compare                      Main     10.2    0.0
egRandom                     Main      2.2    0.9
-}

type Selection = I.IntMap Int
newtype Value = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

data Solution = Solution {selection :: Selection, totalWeight :: !Weight, totalValue :: !Value}
    deriving (Eq, Show)

emptySoln = Solution I.empty 0 0

instance Ord Solution where
    compare = comparing totalValue

main = print =<< egRandom 10000

egRandom n = do
    (cap:xs) <- randomRs (1,1000) <$> newStdGen
    let vws = take n $ pair xs
    return (solve vws cap)
    where
    pair :: [a] -> [(a,a)]
    pair (x1:x2:xs) = (x1,x2) : pair xs
    pair _ = []

-- | Given a list of (Value, Weight) pairs, maximises the total value
--   while keeping the total weight under the capacity.
solve :: [(Int, Int)] -> Int -> Solution
solve vws cap = knapsackNative (V.fromList $ map (V *** W) vws) (W cap)

knapsackNative :: V.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (V.map (second scale) vws) (scale cap)
    where
    -- the gcd of the weights
    gcdW = V.foldl1' gcd $ V.map snd vws
    scale x = div x gcdW
    unscale (Solution s w v) = Solution s (w*gcdW) v

knapsackScaled :: V.Vector (Value, Weight) -> Weight -> Solution
knapsackScaled vws (W cap) = solns V.! cap
    where
    solns :: V.Vector Solution
    solns = V.generate (cap+1) genI

    genI :: Int -> Solution
    genI 0 = emptySoln
    genI i = safeMaximum $ catMaybes $ V.toList $ V.imap eachPair vws
        where
        eachPair :: Int -> (Value, Weight) -> Maybe Solution
        eachPair j (v, w) | unW w <= i = case solns V.! (i - unW w) of
            Solution s' w' v' -> Just $ Solution (I.insertWith (+) j 1 s') (w' + w) (v' + v)
        eachPair _ _ = Nothing

safeMaximum :: [Solution] -> Solution
safeMaximum [] = emptySoln
safeMaximum xs = maximum xs