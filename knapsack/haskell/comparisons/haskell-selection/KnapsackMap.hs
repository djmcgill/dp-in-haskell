{-# LANGUAGE BangPatterns
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , MultiParamTypeClasses
           , StandaloneDeriving
           #-}

module KnapsackMap where

import           Control.Arrow                    (second)
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Lazy
import           Data.Attoparsec.ByteString.Char8 hiding (eitherResult, parse, skipWhile, takeWhile1)
import qualified Data.ByteString.Lazy             as B
import qualified Data.IntMap                      as I
import           Data.List
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Unboxed.Mutable      as UM
import qualified Data.Vector.Generic.Base         as B
import qualified Data.Vector.Generic.Mutable      as M
import           Text.Printf

main = do
    (cap, n, vws) <- readProblem "test_problem_1.data"
    let Solution s (V v) (W w) = knapsackNative (U.fromListN n vws) cap

    printf "\nThe Haskell solution is has a total weight of %i, a total value of %i and a selection of:\n" w v
    forM_ (I.toList s) (uncurry (printf "\tindex: %i, quantity: %i\n"))

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
    where
    makeVW v w = (V v, W w)

-- set up the newtypes including unboxed vectors of them
newtype Value  = V {unV :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
newtype Weight = W {unW :: Int} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, U.Unbox)
deriving instance (B.Vector U.Vector Value)
deriving instance (M.MVector UM.MVector Value)
deriving instance (B.Vector U.Vector Weight)
deriving instance (M.MVector UM.MVector Weight)

-- TODO: test [Int] vs IntMap Int
type Selection = I.IntMap Int
emptySelection = I.empty
addIndex :: Int -> Selection -> Selection
addIndex i = I.insertWith (+) i 1

data Solution = Solution
    { selection   :: Selection
    , totalValue  :: {-# UNPACK #-} !Value
    , totalWeight :: {-# UNPACK #-} !Weight
    } deriving (Eq, Show)

-- the best solution has the highest value or, in the case of a draw, the lowest weight
instance Ord Solution where
    compare = comparing totalValue <> comparing (Down . totalWeight)

emptySoln :: Solution
emptySoln = Solution emptySelection 0 0

knapsackNative :: U.Vector (Value, Weight) -> Weight -> Solution
knapsackNative vws cap = unscale $ knapsackScaled (scaleV vws) (scale cap)
    where
    gcdW    = U.foldl' gcd cap $ U.map snd vws
    scale x = div x gcdW
    scaleV  = if gcdW /= 1 then U.map (second scale) else id
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
              --
      -- i -> maximum (imap eachPair vws)
        !i -> V.foldl' maxMaybe emptySoln $ V.generate (U.length vws) $ \j -> eachPair j (vws U.! j)
                where
                eachPair :: Int -> (Value, Weight) -> Maybe Solution
                eachPair !j (v, w) | unW w <= i = case solns V.! (i - unW w) of
                    Solution s' v' w' -> Just $ Solution (addIndex j s') (v' + v) (w' + w)
                eachPair _ _ = Nothing

                maxMaybe s sM = maybe id max sM s
