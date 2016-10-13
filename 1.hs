import System.Environment
import Data.List


main :: IO ()
main = print $ sumOfMultiples [3, 5] 1000


sumOfMultiples xs limit =
  sum . nub . concat $ [[ i * x | i <- [1..((limit - 1) `div` x)]] | x <- xs]

