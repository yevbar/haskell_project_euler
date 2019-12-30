sumOfSquares :: Int -> Int -> Int
sumOfSquares a b = sum [x ^ 2 | x <- [a..b]]

squareOfSum :: Int -> Int -> Int
squareOfSum a b = (sum [x | x <- [a..b]]) ^ 2

solution :: Int -> Int -> Int
solution a b = (squareOfSum a b) - (sumOfSquares a b)

main = do
  print (solution 1 100) 
