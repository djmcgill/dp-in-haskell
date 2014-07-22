{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , StandaloneDeriving
           #-}

module KnapsackTakeWhile where

import           Control.Arrow                    (second)
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.IntMap.Strict               as I
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Text.Printf
import           System.Environment               (getArgs, getProgName)
import           System.Exit                      (exitSuccess)

-- Parsing imports
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy             as B

-- Vector imports
import qualified Data.Vector                      as V
import qualified Data.Vector.Algorithms.Heap      as H

main = do
    (cap, n, vws) <- readProblem "C:\\code\\phd\\dp-in-haskell\\knapsack\\test_problem_1.data"
    when (V.null vws) $ error "no value/weights found"
    let Solution s (V v) (W w) = knapsackNative vws cap
    return (I.toList (countIxs s), v, w)

printSolution :: Solution -> IO ()
printSolution (Solution s (V v) (W w)) = do
    printf "\nThe Haskell solution is has a total weight of %i, a total value of %i and a selection of:\n" w v
    forM_ (I.toList (countIxs s)) (uncurry (printf "\tindex: %i, quantity: %i\n"))

countIxs :: [Int] -> I.IntMap Int
countIxs = foldl' (\m i -> I.insertWith (+) i 1 m) I.empty

-- Parse the problem from a file
readProblem :: String -> IO (Weight, Int, V.Vector VW)
readProblem filename = do
    file <- B.readFile filename
    either error return $ eitherResult $ flip parse file $ do
        (cap, n) <- decPair
        !vws <- V.forM (V.enumFromN 0 n) $ \i -> do
            (v,w) <- decPair
            return $! VW i (V v) (W w)
        return (W cap, n, vws)
    where
    decPair = do
        d1 <- decimal
        space
        d2 <- decimal
        endOfLine
        return (d1, d2)

-- set up the newtypes including unboxed vectors of them
newtype Value  = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, NFData)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, NFData)

data Solution = Solution
    { selection   :: [Int]                  -- a list of chosen indices
    , totalValue  :: {-# UNPACK #-} !Value  -- the total value of the selection
    , totalWeight :: {-# UNPACK #-} !Weight -- the total weight of the selection
    } deriving (Eq, Show)

data VW = VW
    { originalIx :: Int
    , value      :: {-# UNPACK #-} !Value
    , weight     :: {-# UNPACK #-} !Weight
    } deriving (Eq, Show)

-- the best solution has the highest value or, in the case of a draw, the lowest weight
instance Ord Solution where
    compare = comparing totalValue <> comparing (Down . totalWeight)

emptySoln :: Solution
emptySoln = Solution [] 0 0

-- sort out the params (scaling the weight and sorting the vws) before calling knapsackScaled
knapsackNative :: V.Vector VW -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (scaleVW (sortWeights vws)) (div cap gcdW)
    where
    !gcdW                    = V.foldl' gcd cap $ V.map weight vws
    scaleWeight (VW ix v w)  = VW ix v (div w gcdW)
    scaleVW                  = if gcdW /= 1 then V.map scaleWeight else id
    unscale (Solution s v w) = Solution s v (w*gcdW)
    sortWeights              = V.modify $ H.sortBy (comparing weight)

-- actually compute the solution
knapsackScaled :: V.Vector VW -> Weight -> Solution
knapsackScaled vws (W cap) = V.unsafeIndex solns cap
    where
    -- a vector of the (memoised) best solution at each (scaled) weight
    solns :: V.Vector Solution
    solns = V.generate (cap+1) $ \i ->
        let eachPair :: VW -> Solution
            eachPair (VW ix v w) = case solns `V.unsafeIndex` (i - unW w) of
                Solution s' v' w' -> Solution (ix : s') (v' + v) (w' + w)

            valid vw = weight vw <= W i
        in V.foldl' max emptySoln $ V.map eachPair $ V.takeWhile valid vws
