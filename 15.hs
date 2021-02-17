main = do
  putStrLn . show . numPaths $ 20

choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

numPaths :: Int -> Int
numPaths n = (2 * n) `choose` n
