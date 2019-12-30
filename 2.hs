-- getSum takes two values in Fibonacci sequence and returns sum
-- of Fibonacci values until first parameter is >= 4,000,000
getSum :: Int -> Int -> Int
getSum a b | (a >= 4000000) = 0
           | (mod a 2 == 1) = getSum b (a+b)
           | otherwise = a + (getSum b (a+b))

main = do
  print(getSum 1 2)
