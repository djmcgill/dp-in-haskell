{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , StandaloneDeriving
           #-}

module KnapsackTraverse where

import           Control.Arrow                    (second)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.IntMap.Strict               as I
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Text.Printf
import qualified Data.Traversable                 as T
import           System.Environment               (getArgs, getProgName)
import           System.Exit                      (exitSuccess)

-- Parsing imports
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy             as B

-- Vector imports
import qualified Data.Vector                      as V
import qualified Data.Vector.Algorithms.Heap      as H

import Debug.Trace

main = do
    (cap, n, vws) <- readProblem "C:\\code\\phd\\dp-in-haskell\\knapsack\\test_problem_1.data"
    when (V.null vws) $ error "no value/weights found"
    let Solution s (V v) (W w) = knapsackNative (V.indexed vws) cap
    return (I.toList (countIxs s), v, w)

printSolution :: Solution -> IO ()
printSolution (Solution s (V v) (W w)) = do
    printf "\nThe Haskell solution is has a total weight of %i, a total value of %i and a selection of:\n" w v
    forM_ (I.toList (countIxs s)) (uncurry (printf "\tindex: %i, quantity: %i\n"))

countIxs :: [Int] -> I.IntMap Int
countIxs = foldl' (\m i -> I.insertWith (+) i 1 m) I.empty

-- Parse the problem from a file
readProblem :: String -> IO (Weight, Int, V.Vector (Value, Weight))
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        (cap, n) <- decPair
        vws <- V.replicateM n $ do
            (v,w) <- decPair
            return (V v, W w)
        return (W cap, n, vws)
    where
    decPair = do
        d1 <- decimal
        space
        d2 <- decimal
        endOfLine
        return (d1, d2)

-- set up the newtypes including unboxed vectors of them
newtype Value  = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

getW :: (Int, (Value, Weight)) -> Weight
getW = snd . snd

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

-- sort out the params (scaling the weight and sorting the vws) before calling knapsackScaled
knapsackNative :: V.Vector (Int, (Value, Weight)) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (scaleV (sortWeights vws)) (scale cap)
    where
    gcdW    = V.foldl' gcd cap $ V.map getW vws
    scale x = div x gcdW
    scaleV  = if gcdW /= 1 then V.map (second (second scale)) else id
    unscale (Solution s v w) = Solution s v (w*gcdW)
    sortWeights = V.modify $ H.sortBy (comparing getW)

-- actually compute the solution
knapsackScaled :: V.Vector (Int, (Value, Weight)) -> Weight -> Solution
knapsackScaled vws (W cap) = V.unsafeIndex solns cap
    where
    -- a vector of the (memoised) best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = snd $ T.mapAccumL go 0 $ V.enumFromN 0 (cap+1)
        where
        go :: Int -> Int -> (Int, Solution)
        go !oldIx !i = (newIx, best)
            where
            eachPair :: (Int, (Value, Weight)) -> Solution
            eachPair (ix, (v, w)) = case solns `V.unsafeIndex` (i - unW w) of
                Solution s' v' w' -> Solution (ix : s') (v' + v) (w' + w)
            newIx = oldIx + V.length (V.takeWhile valid (V.unsafeDrop oldIx vws))
            valid :: (Int, (Value, Weight)) -> Bool
            valid = (i >=) . unW . getW
            best = V.foldl' max emptySoln $ V.map eachPair $ V.unsafeTake newIx vws
