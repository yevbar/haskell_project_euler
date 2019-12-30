isPrime :: Int -> Int -> Bool
isPrime cur n | (cur > floor (sqrt (fromIntegral n))) = True
              | (mod n cur == 0) = False
              | otherwise = isPrime (cur+1) n

getSumOfPrimesLessThanN :: Int -> Int -> Int -> Int
getSumOfPrimesLessThanN cur sum n | (cur == n) = sum
                                  | (isPrime 2 cur) = getSumOfPrimesLessThanN (cur+1) (sum+cur) n
                                  | otherwise = getSumOfPrimesLessThanN (cur+1) sum n

main = do
  print (getSumOfPrimesLessThanN 2 0 2000000)
