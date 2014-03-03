gcds :: Int -> [Int] -> Int
gcds capacity weights = foldr1 gcd (capacity:weights)