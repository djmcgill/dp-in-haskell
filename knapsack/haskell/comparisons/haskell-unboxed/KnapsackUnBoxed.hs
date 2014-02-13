{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, LambdaCase, StandaloneDeriving,
             MultiParamTypeClasses, TypeFamilies #-}

module KnapsackUnBoxed (solve) where

import           Control.Arrow         (second, (***))
import qualified Data.IntMap.Lazy as I
import           Data.Monoid           ((<>))
import           Data.Ord              (comparing, Down(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic.Base as B
import qualified Data.Vector.Generic.Mutable as M

type Selection = I.IntMap Int
newtype Value = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)

deriving instance (B.Vector U.Vector Value)
deriving instance (M.MVector UM.MVector Value)

deriving instance (B.Vector U.Vector Weight)
deriving instance (M.MVector UM.MVector Weight)


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
    where Solution s (V v) (W w) = knapsackNative (U.map (V *** W) (U.fromList vws)) (W cap)

knapsackNative :: U.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled vws' (scale cap)
    where
    vws' = if gcdW /= 1 then (U.map (second scale) vws) else vws
    gcdW = U.foldl' gcd cap $ U.map snd vws
    scale x = div x gcdW
    unscale (Solution s v w) = Solution s v (w*gcdW)

knapsackScaled :: U.Vector (Value, Weight) -> Weight -> Solution
knapsackScaled vws (W cap) = solns V.! cap
    where
    -- a vector of the best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = V.generate (cap+1) $ \case
        0  -> emptySoln
              -- TODO: if vws is sorted by weight then we can go only up to
              --       the max allowed weight rather than go through the whole
              --       thing each time
        !i -> V.foldl' maxMaybe emptySoln (V.imap eachPair (U.convert vws))
                where
                eachPair :: Int -> (Value, Weight) -> Maybe Solution
                eachPair !j (v, w) | unW w <= i = case solns V.! (i - unW w) of
                    Solution s' v' w' -> Just $ Solution (I.insertWith (+) j 1 s') (v' + v) (w' + w)
                eachPair _ _ = Nothing

                maxMaybe sM s = maybe id max sM s
