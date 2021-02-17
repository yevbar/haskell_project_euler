import Data.Maybe (fromJust)
import Data.Map (Map, fromList, member, insert)
import qualified Data.Map as Map

main = do
  putStrLn . show . findLongestSeq $ 1000000

collatz :: Map Int Int -> Int -> Map Int Int
collatz store value
  | member value store = store
  | otherwise =
    let next = if mod value 2 == 0 then div value 2 else 3 * value + 1
        newStore = collatz store next
        result = 1 + (fromJust $ Map.lookup next newStore)
    in insert value result newStore

controller :: Map Int Int -> Int -> Int -> Int -> Int
controller store largest cur limit
  | cur == limit = largest
  | otherwise =
    let newStore = collatz store cur
        curValue = fromJust $ Map.lookup cur newStore
        newLargest = if curValue > (fromJust (Map.lookup largest newStore)) then cur else largest
    in controller newStore newLargest (cur + 1) limit

findLongestSeq :: Int -> Int
findLongestSeq limit = controller (fromList [(1, 1)]) 1 2 limit
