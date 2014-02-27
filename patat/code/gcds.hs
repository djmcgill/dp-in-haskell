import Data.Vector
import Prelude hiding (foldr)

gcds :: (Int, Vector Int) -> Int
gcds (capacity, weights) = foldr gcd capacity weights