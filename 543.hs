import System.Environment
import qualified Data.Set as Set
import Data.List
import Debug.Trace

main = print $ s 1000

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


-- fibs = unfoldr (\(a, b) -> Just (a, (b, a+b))) (0,1)

s :: Int -> Int
s n =
  let
    s' :: Int -> Int
    s' = (map s'' [0 ..] !!)
      where
        s'' 0 = 0
        s'' n = sum [ (p k n) | k <- [1..(n `div` 2)] ] + s' (n - 1)


    p :: Int -> Int -> Int
    -- p k n  | trace ("p " ++ show k) False = undefined
    p k n  = if elem n (allSums k) then 1 else 0


    allSums :: Int -> [Int]
    allSums = (map sums [0 .. ] !!)
       where
         sums :: Int -> [Int]
         -- sums k | trace ("sums " ++ show k ) False = undefined
         sums 1 = possiblePrimes
         sums k = nub [ x + a | x <- possiblePrimes,
                                a <- (allSums (k - 1)),
                                x + a <= n
                      ]

    possiblePrimes :: [Int]
    possiblePrimes = takeWhile (n >=) primes
  in s' n
