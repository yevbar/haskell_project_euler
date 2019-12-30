isPrime :: Int -> Bool
isPrime n = length [x | x <- [2..(ceiling (sqrt (fromIntegral n)))], mod n x == 0] == 0

getNthPrimeNumber :: Int -> Int -> Int -> Int
getNthPrimeNumber num cur n | (cur == n - 1 && isPrime num) = num
                            | (isPrime num) = getNthPrimeNumber (num + 1) (cur + 1) n
                            | otherwise = getNthPrimeNumber (num + 1) cur n

main = do
  print (getNthPrimeNumber 2 1 10001)
