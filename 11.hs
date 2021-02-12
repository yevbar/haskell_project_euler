{-# LANGUAGE OverloadedStrings #-}

import Data.List (tails, transpose)
import Data.Universe.Helpers (diagonals)

main = do
  input <- readFile "inputs/11.txt"
  findLargestProduct $ gridify input

atoi :: String -> Int
atoi str = read str :: Int

gridify :: String -> [[Int]]
gridify str = [map atoi (words line) | line <- lines str]

takeEveryN :: Int -> [a] -> [[a]]
takeEveryN n arr = zipWith const (fmap (take n) (tails arr)) (drop (n-1) arr)

takeFirstN :: Int -> [a] -> [a]
takeFirstN n arr = zipWith const arr (drop n arr)

getDiagonals :: [[Int]] -> [[Int]]
getDiagonals arr = takeFirstN (length arr - 1) (drop (length arr - 1) (diagonals arr))

getAdjacentNumbers :: [[Int]] -> [[Int]]
getAdjacentNumbers arr =
  -- Get elements that are adjacent horizontally
  concat [takeEveryN 4 row | row <- arr] ++
  -- Get elements that are adjacent vertically
  concat [takeEveryN 4 row | row <- transpose arr] ++
  -- Get elements that are adjacent diagonally (bottom left to upper right)
  concat [getDiagonals rows | rows <- takeEveryN 4 arr] ++
  -- Get elements that are adjacent diagonally (bottom right to upper left)
  concat [getDiagonals rows | rows <- takeEveryN 4 (transpose arr)]

findLargest :: [[Int]] -> Int -> Int
findLargest [] _default = _default
findLargest (x:xs) _default =
  let product = foldl (*) 1 x
  in if product > _default
     then (findLargest xs product)
     else (findLargest xs _default)

findLargestProduct :: [[Int]] -> IO ()
findLargestProduct arr = print $ findLargest (getAdjacentNumbers arr) 1
