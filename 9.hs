import Debug.Trace

isPythagoreanTriple :: Int -> Int -> Int -> Bool
isPythagoreanTriple a b c = (a^2 + b^2) == c^2

getRange :: Int -> [Int]
getRange n = [1..n]

findTripleWithSum :: Int -> Int
findTripleWithSum sum = head [a*b*c | a <- getRange sum, b <- getRange sum, c <- getRange sum, a + b + c == sum, isPythagoreanTriple a b c]

main = do
  print (findTripleWithSum 1000)
