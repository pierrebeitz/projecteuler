import System.Environment
import Data.List


main :: IO ()
main = print fibs

fibs =
  foldr (+) 0
  $ filter even
  $ takeWhile (<4000000)
  $ unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)
