import Data.Vector
import Prelude hiding (foldr)

gcds :: Int -> Vector Int -> Int
gcds capacity weights = foldl' gcd capacity weights