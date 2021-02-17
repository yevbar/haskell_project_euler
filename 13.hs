import Data.List (intercalate)

import Debug.Trace (trace)

main = do
  input <- readFile "inputs/13.txt"
  let values = [atoi line | line <- lines input]
  putStrLn . take 10 $ show . sum $ values

atoi :: String -> Integer
atoi num = read num :: Integer
