{-# LANGUAGE BangPatterns #-}
import System.Random (randomRIO)

import Data.List (sort)
-- 1.3
sumTwoLargestSquares :: (Num a, Ord a) => a -> a -> a -> a
sumTwoLargestSquares a b c = sum $ map (\x -> x * x) largest
  where largest = tail $ sort [a, b, c]

-- 1.8
newtonCubeRoot :: Double -> Double -> Double
newtonCubeRoot guess x | isGoodEnough = guess
                       | otherwise = newtonCubeRoot improved x
  where isGoodEnough = abs (cube guess - x) < 0.001
        improved = ((x / square guess) + 2 * guess) / 3
        cube = (** 3)
        square = (** 2)

-- 1.11
fRecursive :: Integer -> Integer
fRecursive n | n < 3     = n
             | otherwise = let f = fRecursive
                           in f (n - 1) + 2 * f (n - 2) + 3 * f (n - 3)

fIterative :: Integer -> Integer
fIterative n | n < 3     = n
             | otherwise = go 2 1 0 3
  where go !fn1 !fn2 !fn3 !x | x > n     = fn1
                             | otherwise =
                               go (fn1 + 2 * fn2 + 3 * fn3) fn1 fn2 (x + 1)

-- 1.12
pascal :: [[Integer]]
pascal = go [1]
  where go prev = let row = 1:f prev
                  in prev:go row
        f (a:r@(b:_)) = a + b:f r
        f _ = [1]

-- 1.16
iterExp :: Integer -> Integer -> Integer
iterExp b0 n0 = go 1 b0 n0
  where go !a !b !n | n == 0    = a
                    | even n    = go a (b * b) (div n 2)
                    | otherwise = go (a * b) b (n - 1)

-- 1.17
recMul :: Integer -> Integer -> Integer
recMul a b | b == 0    = 0
           | even b    = recMul (double a) (halve b)
           | otherwise = a + recMul a (b - 1)
  where double x = x + x
        halve x = x `div` 2

-- 1.18
iterMul :: Integer -> Integer -> Integer
iterMul a0 b0 = go 0 a0 b0
  where double x = x + x
        halve x = x `div` 2
        go !n !a !b | b == 0    = n
                    | even b    = go n (double a) (halve b)
                    | otherwise = go (a + n) a (b - 1)
-- 1.19
fibIter :: Integer -> Integer
fibIter n = go 1 0 0 1 n
  where go !a !b !p !q !count
          | count == 0 = b
          | even count = let p1 = (p * p) + (q * q)
                             q1 = q * (q + 2 * p)
                         in go a b p1 q1 (count `div` 2)
          | otherwise  = let a1 = (b * q) + (a * q) + (a * p)
                             b1 = (b * p) + (a * q)
                         in go a1 b1 p q (count - 1)

-- 1.27
carmichaelsTest :: [(Integer, Bool)]
carmichaelsTest = map (\n -> (n, f n)) carmichaels
  where f n = let t a = expMod a n n == a `mod` n
              in all t [1..n-1]
        carmichaels = [561, 1105, 1729, 2465, 2821, 6601]

-- 1.28
expMod :: Integer -> Integer -> Integer -> Integer
expMod base ex m | ex == 0   = 1
                 | even ex   = square (expMod base (ex `div` 2) m) `mod` m
                 | otherwise = base * (expMod base (ex - 1) m) `mod` m
  where square x = x * x

fermatTest :: Integer -> IO Bool
fermatTest n = do
  a <- randomRIO (1, n - 1)
  return $ expMod a n n == a

isPrimeFast :: Integer -> Integer -> IO Bool
isPrimeFast n times | times == 0 = return True
                    | otherwise = do
                      guess <- fermatTest n
                      if guess
                        then isPrimeFast n (times - 1)
                        else return False

expModMR :: Integer -> Integer -> Integer -> Integer
expModMR base ex m | ex == 0   = 1
                   | even ex   =
                     failFast $ square (expMod base (ex `div` 2) m) `mod` m
                   | otherwise = base * (expMod base (ex - 1) m) `mod` m
  where square x = x * x
        failFast n = if n == 1 then 1 else 0

millerRabinTest :: Integer -> IO Bool
millerRabinTest n = do
  a <- randomRIO (1, n - 1)
  return $ expModMR a (n - 1) n == 1

isPrimeMR :: Integer -> Integer -> IO Bool
isPrimeMR n times | times == 0 = return True
                  | otherwise = do
                    guess <- millerRabinTest n
                    if guess
                      then isPrimeMR n (times - 1)
                      else return False

primesTo :: Integer -> [Integer]
primesTo m = 2 : eratos [3,5..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..m])
   minus (x:xs) (y:ys) = case (compare x y) of
     LT -> x : minus  xs  (y:ys)
     EQ ->     minus  xs     ys
     GT ->     minus (x:xs)  ys
   minus  xs     _     = xs

-- 1.29