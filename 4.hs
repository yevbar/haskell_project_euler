numDigits :: Int -> Int
numDigits n = fromIntegral (round (logBase 10 (fromIntegral n)) + 1)

getPowerOfTen :: Int -> Int
getPowerOfTen n = floor(10 ^ (floor (logBase 10 (fromIntegral n))))

notEqual :: Char -> Char -> Bool
notEqual a b | (a == b) = False
             | otherwise = True

isPalindromeString :: String -> Bool
isPalindromeString s | (elem (length s) [0,1]) = True
                     | (notEqual (head s) (last s)) = False
                     | otherwise = isPalindromeString (init (tail s))

isPalindrome :: Int -> Bool
isPalindrome n = isPalindromeString (show n)

maxValue :: [Int] -> Int -> Int
maxValue arr currentMax | (length arr == 0) = currentMax
                       | ((head arr) > currentMax) = maxValue (tail arr) (head arr)
                       | otherwise = maxValue (tail arr) currentMax

-- getLargestPalindrome takes in min/max values and returns maximum product that's a palindrome
getLargestPalindrome :: Int -> Int -> Int
getLargestPalindrome a b = maxValue [(x*y) | x <- [a..b], y <- [a..b], isPalindrome (x*y)] 0

main = do
  print (getLargestPalindrome 100 999)
