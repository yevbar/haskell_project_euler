{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace (trace)

main = do
  putStrLn $ show $ getTriangleNumberWithDivisors 500

handleDivisor :: Int -> Int -> Int
handleDivisor a b
  | div a b == b = 1
  | otherwise = 2

numDivisors :: Int -> Int
numDivisors n =
  sum [handleDivisor n x | x <- [1..(floor . sqrt . fromIntegral $ n)], mod n x == 0]

checkNum :: Int -> Int -> Bool
checkNum goal num = (numDivisors num) > goal

findNum :: Int -> Int -> Int -> Int
findNum goal cur index
  | checkNum goal cur = cur
  | otherwise = findNum goal (cur+index) (index+1)

getTriangleNumberWithDivisors goal = findNum goal 1 2
