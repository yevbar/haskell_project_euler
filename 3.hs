isPrime :: Int -> Bool
isPrime n = length [x | x <- [2..n], mod n x == 0] == 1

getLargestFactor :: Int -> Int -> Int
getLargestFactor a b | (b == 1) = 1
                     | (mod a b == 0 && isPrime b) = b
                     | otherwise = getLargestFactor a (b-1)

-- You only need to start at sqrt(n) and work down to
-- find largest factor
largestPrimeFactor :: Int -> Int
largestPrimeFactor n = getLargestFactor n (floor(sqrt (fromIntegral n)))

main = do
  print (largestPrimeFactor 600851475143)
