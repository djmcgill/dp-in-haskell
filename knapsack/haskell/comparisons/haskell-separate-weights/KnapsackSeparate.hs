{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , StandaloneDeriving
           #-}

module KnapsackSeparate where

import           Control.Arrow                    (second)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import qualified Data.IntMap.Strict               as I
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Text.Printf

-- Parsing imports
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy             as B

-- Vector imports
import qualified Data.Vector                      as V
import qualified Data.Vector.Algorithms.Heap      as H
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as UM
import qualified Data.Vector.Generic              as G
import qualified Data.Vector.Generic.Mutable      as GM

-- TODO: don't actually need to sort Values as long as we keep each weight with its index

main = do
    (cap, n, vs, ws) <- readProblem "..\\test_problem_1.data"
    when (U.null vs || U.null ws) $ error "no value/weights found"
    let Solution s (V v) (W w) = (knapsackNative vs (U.indexed ws) cap)
    return (s, v, w)

printSolution :: Solution -> IO ()
printSolution (Solution s (V v) (W w)) = do
    printf "\nThe Haskell solution is has a total weight of %i, a total value of %i and a selection of:\n" w v
    forM_ (I.toList (countIxs s)) (uncurry (printf "\tindex: %i, quantity: %i\n"))
    where countIxs = foldl' (\m i -> I.insertWith (+) i (1 :: Int) m) I.empty

-- Parse the problem from a file
readProblem :: String -> IO (Weight, Int, U.Vector Value, U.Vector Weight)
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        cap <- decimal
        space
        n <- decimal
        endOfLine
        vws <- U.replicateM n $ do
            v <- decimal
            space
            w <- decimal
            endOfLine
            return (V v, W w)
        let (vs, ws) = U.unzip vws
        return (W cap, n, vs, ws)

-- set up the newtypes including unboxed vectors of them
newtype Value  = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
deriving instance (G.Vector   U.Vector   Value)
deriving instance (GM.MVector UM.MVector Value)
deriving instance (G.Vector   U.Vector   Weight)
deriving instance (GM.MVector UM.MVector Weight)

data Solution = Solution
    { selection   :: [Int]                  -- a list of chosen indices
    , totalValue  :: {-# UNPACK #-} !Value  -- the total value of the selection
    , totalWeight :: {-# UNPACK #-} !Weight -- the total weight of the selection
    } deriving (Eq, Show)

-- the best solution has the highest value or, in the case of a draw, the lowest weight
instance Ord Solution where
    compare = comparing totalValue <> comparing (Down . totalWeight)

emptySoln :: Solution
emptySoln = Solution [] 0 0

knapsackNative :: U.Vector Value -> U.Vector (Int, Weight) -> Weight -> Solution
knapsackNative vs iws cap = unscale $ knapsackScaled vs (scaleV (sortWeights iws)) (scale cap)
    where
    gcdW    = U.foldl' gcd cap $ U.map snd iws
    scale x = div x gcdW
    scaleV  = if gcdW /= 1 then U.map (second scale) else id
    unscale (Solution s v w) = Solution s v (w*gcdW)

    sortWeights = U.modify (H.sortBy (comparing snd))

knapsackScaled :: U.Vector Value -> U.Vector (Int, Weight) -> Weight -> Solution
knapsackScaled vs iws (W cap) = V.unsafeIndex solns cap
    where
    -- a vector of the best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = V.unfoldrN (cap+1) getBest (0,0)
        where
        -- i is the current index, less is the maximal index into vws such that forall ix. 0 <= ix < less', snd (iws[ix]) <= W i
        getBest (!i, !less) = Just (maxSolution, (i+1, less'))
            where
            maxSolution = V.foldl' max emptySoln $ V.generate (U.length validWeights) (eachPair . U.unsafeIndex validWeights)
            -- validWeights = U.takeWhile (\x -> snd x <= W i) iws
            -- but takes advantage of the fact that the slice is strictly non-decreasing to memoise
            -- TODO: benchmark this vs U.takeWhile
            validWeights = U.unsafeTake less' iws
            less' = updateLess less
                where
                maxIx = U.length iws
                updateLess j = if j == maxIx || snd (U.unsafeIndex iws j) > W i
                                then j else updateLess (j+1)

            -- This is what the solution would look like with the vw pair in.
            -- Please don't call it in a case where i < w or bad things will happen.
            eachPair :: (Int, Weight) -> Solution
            eachPair (ix, w) = case solns `V.unsafeIndex` (i - unW w) of
                Solution s' v' w' -> Solution (ix : s') (v' + v) (w' + w)
                where v = vs `U.unsafeIndex` ix
