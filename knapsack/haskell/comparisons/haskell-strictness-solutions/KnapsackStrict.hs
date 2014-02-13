{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module KnapsackStrict (solve) where

import           Control.Arrow       (second, (***))
import qualified Data.IntMap.Strict as I -- TODO: benchmark being strict
import           Data.List           (foldl')
import           Data.Maybe          (catMaybes)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing, Down(..))
import qualified Data.Vector as V

{- TODO:
talk about
    lazy vs strict in Solution
-}

type Selection = I.IntMap Int
newtype Value = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

data Solution = Solution {
    selection :: Selection,
    totalValue :: {-# UNPACK #-} !Value,
    totalWeight :: {-# UNPACK #-} !Weight}
    deriving (Eq, Show)

emptySoln = Solution I.empty 0 0

-- the best solution has the highest value or, in the case of a draw, the lowest weight
instance Ord Solution where
    compare = comparing totalValue <> comparing (Down . totalWeight)

-- | Given a list of (Value, Weight) pairs (where the weights are positive), maximises the total value
--   while keeping the total weight under the capacity.
solve :: [(Int, Int)] -> Int -> (I.IntMap Int, Int, Int)
solve vws cap = (s, v, w)
    where Solution s (V v) (W w) = knapsackNative (V.fromList $ map (V *** W) vws) (W cap)

knapsackNative :: V.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled vws' (scale cap)
    where
    vws' = if gcdW /= 1 then (V.map (second scale) vws) else vws
    gcdW = V.foldl' gcd cap $ V.map snd vws -- is this fused?
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
    genI !i = foldl' max emptySoln $ catMaybes $ V.toList $ V.imap eachPair vws -- possibly remove the intermediate list here?
        where
        eachPair :: Int -> (Value, Weight) -> Maybe Solution
        eachPair !j (v, w) | unW w <= i = case solns V.! (i - unW w) of
            Solution s' !v' !w' -> Just $ Solution (I.insertWith (+) j 1 s') (v' + v) (w' + w)
        eachPair _ _ = Nothing
