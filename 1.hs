-- getSum returns sum of values less than n
-- that are multiples of five of three
getSum :: Int -> Int
getSum 0 = 0
getSum n | (mod n 3 == 0) = n + getSum(n-1)
         | (mod n 5 == 0) = n + getSum(n-1)
         | otherwise    = getSum(n-1)

main = do
  -- We provide 1000-1 so that 1000 doesn't get
  -- included in final sum
  print (getSum 999)
