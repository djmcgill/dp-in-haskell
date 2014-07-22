{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , MultiParamTypeClasses
           , StandaloneDeriving
           #-}

module KnapsackSortedGenerate where

import           Control.Arrow                    (second)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy             as B
import qualified Data.IntMap.Strict               as I
import qualified Data.List                        as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector                      as V
import qualified Data.Vector.Algorithms.Heap      as H
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as UM
import qualified Data.Vector.Generic.Base         as B
import qualified Data.Vector.Generic.Mutable      as M
import           Text.Printf

main = do
    (cap, n, vws) <- readProblem "test_problem_1.data"
    let Solution s (V v) (W w) = knapsackNative (U.fromListN n vws) cap
    return (I.toList (countIxs s), v, w)

    where countIxs = L.foldl' (\m i -> I.insertWith (+) i 1 m) I.empty

-- TODO: make this insert vws into a mutable vector? Can you fuse parsing and vectoring?
readProblem :: String -> IO (Weight, Int, [(Value,Weight)])
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        cap <- decimal
        space
        n <- decimal
        endOfLine
        vws <- many $ (makeVW <$> decimal <* space <*> decimal <* endOfLine)
        return (W cap, n, vws)
    where makeVW v w = (V v, W w)

-- set up the newtypes including unboxed vectors of them
newtype Value  = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
deriving instance (B.Vector  U.Vector   Value)
deriving instance (M.MVector UM.MVector Value)
deriving instance (B.Vector  U.Vector   Weight)
deriving instance (M.MVector UM.MVector Weight)

data Solution = Solution
    { selection   :: [Int]                  -- a list of chosen indicies
    , totalValue  :: {-# UNPACK #-} !Value  -- the total value of the selection
    , totalWeight :: {-# UNPACK #-} !Weight -- the total weight of the selection
    } deriving (Eq, Show)

-- the best solution has the highest value or, in the case of a draw, the lowest weight
instance Ord Solution where
    compare = comparing totalValue <> comparing (Down . totalWeight)

emptySoln :: Solution
emptySoln = Solution [] 0 0

knapsackNative :: U.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (unsafeSort (scaleV vws)) (scale cap)
    where
    gcdW    = U.foldl' gcd cap $ U.map snd vws
    scale x = div x gcdW
    scaleV  = if gcdW /= 1 then U.map (second scale) else id
    unscale (Solution s v w) = Solution s v (w*gcdW)
    unsafeSort :: U.Vector (Value, Weight) -> U.Vector (Value, Weight)
    unsafeSort v = runST $ do
      vM <- U.unsafeThaw v
      H.sortBy (comparing snd) vM
      U.unsafeFreeze vM

knapsackScaled :: U.Vector (Value, Weight) -> Weight -> Solution
knapsackScaled vws (W cap) = solns V.! cap
    where
    -- a vector of the best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = V.generate (cap+1) $ \case
        0  -> emptySoln
        -- might even be able to take advantage of the fact that `takeWhile (<=i)' and `takeWhile (<= (i+1))' are so similar
        !i -> V.foldl' max emptySoln $ V.imap eachPair $ V.takeWhile ((<=i) . unW . snd) (V.generate (U.length vws) (vws U.!))
                where
                eachPair :: Int -> (Value, Weight) -> Solution
                eachPair !j (v, w) = case solns V.! (i - unW w) of
                    Solution s' v' w' -> Solution (j : s') (v' + v) (w' + w)

                maxMaybe s sM = maybe id max sM s
