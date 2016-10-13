import System.Environment
import Data.List


main :: IO ()
main = print $ s 100

fibs = unfoldr (\(a, b) -> Just (a, (b, a+b))) (0,1)


-- S


s :: Int -> Int
s n = s_list !! n

s_list :: [Int]
s_list = map (memo_s s) [0..]

memo_s :: (Int -> Int) -> Int -> Int
memo_s ms 0 = 0
memo_s ms n =
  sum [ (p n k) | k <- [1..(n `div` 2)] ] + ms (n - 1)


-- P


p :: Int -> Int -> Int
p n k =
  let
    perms =
      cartesianish n (k - 1) $ (map (\n -> [n]) (possiblePrimes n))
  in
    if elem n (map sum perms) then 1 else 0

cartesianish :: Int -> Int -> [[Int]] -> [[Int]]
cartesianish n 0 arr = arr
cartesianish n k arr =
  cartesianish n (k - 1) [ p : a | p <- possiblePrimes n, a <- arr, (head a) >= p ]

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


possiblePrimes :: Int -> [Int]
possiblePrimes k =
  takeWhile ((>=) k) primes
