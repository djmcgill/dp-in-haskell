import Prelude hiding (gcd)

gcd :: Int -> Int -> Int
gcd 1 _ = 1
gcd 0 b = b
gcd a b = gcd (b `mod` a) a

gcds :: Int -> [Int] -> Int
gcds capacity weights = foldr1 gcd (capacity:weights)
