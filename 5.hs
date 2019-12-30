isPrime :: Int -> Bool
isPrime n = length [x | x <- [1..n], mod n x == 0] == 2

divide :: Int -> Int -> Int
divide a b = floor ((fromIntegral a) / (fromIntegral b))

powerOfFactor :: Int -> Int -> Int
powerOfFactor num fac | ((mod num fac) == 0) = 1 + (powerOfFactor (divide num fac) fac)
                      | otherwise = 0

getPrimeFactors :: Int -> [(Int, Int)]
getPrimeFactors n = [(x, (powerOfFactor n x)) | x <- [2..n], isPrime x, (powerOfFactor n x) > 0]

getPrimePower :: (Int, Int) -> Int
getPrimePower pair = (fst pair) ^ (snd pair)

multiplyFactors :: [(Int, Int)] -> Int
multiplyFactors arr | (length arr == 0) = 1
                    | otherwise = (getPrimePower (head arr)) * multiplyFactors (tail arr)

eitherEmpty :: [(Int, Int)] -> [(Int, Int)] -> Bool
eitherEmpty a b = (length a == 0) || (length b == 0)

nonEmptyOne :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
nonEmptyOne a b | (length a == 0) = b
                | otherwise = a

firstFactor :: [(Int, Int)] -> Int
firstFactor factors = fst (head factors)

firstPower :: [(Int, Int)] -> Int
firstPower factors = snd (head factors)

mergeFactors :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
mergeFactors a b result | (eitherEmpty a b) = result ++ (nonEmptyOne a b)
                        | (firstFactor a < firstFactor b) = mergeFactors (tail a) b (result ++ [head a])
                        | (firstFactor b < firstFactor a) = mergeFactors a (tail b) (result ++ [head b])
                        | (firstPower a > firstPower b) = mergeFactors (tail a) (tail b) (result ++ [head a])
                        | otherwise = mergeFactors (tail a) (tail b) (result ++ [head b])

getAllPrimeFactors :: [Int] -> [[(Int, Int)]]
getAllPrimeFactors arr = [getPrimeFactors x | x <- arr]

mergeAllFactors :: [[(Int, Int)]] -> [(Int, Int)] -> [(Int, Int)]
mergeAllFactors source result | (length source == 0) = result
                              | otherwise = mergeAllFactors (tail source) (mergeFactors (head source) result [])

getMinProduct :: Int -> Int -> Int
getMinProduct a b = multiplyFactors (mergeAllFactors (getAllPrimeFactors [a..b]) [])

main = do
  print (getMinProduct 1 20)
