{-# LANGUAGE BangPatterns #-}
import qualified Data.Ratio as R
import Control.Monad (guard)
import Data.List (sort)

-- 2.1
data Rat = Rat Integer Integer
  deriving (Show)

instance Eq Rat where
  (Rat n0 d0) == (Rat n1 d1) = (n0 == n1 && d0 == d1) || (n0 * d1 == n1 * d0)

instance Ord Rat where
  compare (Rat n0 d0) (Rat n1 d1) = (n0 * d1) `compare` (n1 * d0)

instance Num Rat where
  (Rat n0 d0) + (Rat n1 d1) = makeRat (n0 * d1 + n1 * d0) (d0 * d1)
  (Rat n0 d0) * (Rat n1 d1) = makeRat (n0 * n1) (d0 * d1)
  negate (Rat n d) = Rat (negate n) d
  abs (Rat n d) = Rat (abs n) d
  signum (Rat n _d) = makeRat (signum n) 1
  fromInteger = flip makeRat 1

instance Fractional Rat where
  (Rat n0 d0) / (Rat n1 d1) = makeRat (n0 * d1) (d0 * n1)
  recip (Rat n d) = makeRat d n
  fromRational r = makeRat (R.numerator r) (R.denominator r)

instance Real Rat where
  toRational (Rat n d) = n R.% d

makeRat :: Integer -> Integer -> Rat
makeRat n0 d0 = Rat n d
  where g = gcd n0 d0
        n = signum n0 * signum d0 * abs n0 `div` g
        d = abs d0 `div` g

numer :: Rat -> Integer
numer (Rat n _d) = n

denom :: Rat -> Integer
denom (Rat _n d) = d

-- 2.2
newtype Point a = Point (a, a)
                deriving (Eq, Show)
newtype Segment a = Segment (Point a, Point a)
                deriving (Eq, Show)

makeSegment :: Fractional a => Point a -> Point a -> Segment a
makeSegment = curry Segment

makePoint :: Fractional a => a -> a -> Point a
makePoint = curry Point

avg :: Fractional a => a -> a -> a
avg a b = (a + b) / 2

midpointSegment :: Fractional a => Segment a -> Point a
midpointSegment (Segment (Point (x0, y0), Point (x1, y1))) =
  makePoint (avg x0 x1) (avg y0 y1)

-- 2.3
class Rect a where
  width :: Fractional b => a -> b
  height :: Fractional b => a -> b
  perimeter :: Fractional b => a -> b
  perimeter a = (width a + height a) * 2
  area :: Fractional b => a -> b
  area a = width a * height a

-- Top, Left, Bottom, Right
newtype RectP a = RectP (a, a, a, a)
                deriving (Show, Eq)
-- Origin, Width, Height
newtype RectD a = RectD (Point a, a, a)
                deriving (Show, Eq)

instance (Real a) => Rect (RectP a) where
  width (RectP (_t, l, _b, r)) = fromRational $ toRational (abs (r - l))
  height (RectP (t, _l, b, _r)) = fromRational $ toRational (abs (b - t))

instance (Real a) => Rect (RectD a) where
  width (RectD (_o, w, _h)) = fromRational $ toRational w
  height (RectD (_o, _w, h)) = fromRational $ toRational h

-- 2.5

newtype TPair = TPair Integer
  deriving (Show, Eq)
tCons :: Integer -> Integer -> TPair
tCons a b = TPair $ 2 ^ a * 3 ^ b
tCar :: TPair -> Integer
tCar (TPair a) = tFactor 2 a
tCdr :: TPair -> Integer
tCdr (TPair a) = tFactor 3 a

tFactor :: Integer -> Integer -> Integer
tFactor n = go
  where go a = case a `divMod` n of
          (m, 0) | m > 0     -> 1 + go m
          _                  -> 0

-- 2.6

type ChurchN a = (a -> a) -> a -> a
cZero :: ChurchN a
cZero _f x = x
cAdd1 :: ChurchN a -> ChurchN a
cAdd1 n f x = f (n f x)
cOne :: ChurchN a
cOne f x = f x
cTwo :: ChurchN a
cTwo f x = f (f x)
cAdd :: ChurchN a -> ChurchN a -> ChurchN a
cAdd a b f x = b f (a f x)

-- 2.7 through 2.16

data Interval = Interval { lowerBound :: Double
                         , upperBound :: Double
                         }
  deriving (Show)

makeInterval :: Double -> Double -> Interval
makeInterval a b | a <= b    = Interval a b
                 | otherwise = Interval b a

pairsInterval :: Interval -> Interval -> [(Double, Double)]
pairsInterval (Interval l0 h0) (Interval l1 h1) = [ (l0, l1)
                                                  , (l0, h1)
                                                  , (h0, l1)
                                                  , (h0, h1)
                                                  ]

minmax :: Ord a => [a] -> (a, a)
minmax l = (minimum l, maximum l)

opInterval :: (Double -> Double -> Double) -> Interval -> Interval -> Interval
opInterval op a b = uncurry makeInterval . minmax . map (uncurry op) $ pairs
  where pairs = pairsInterval a b

addInterval :: Interval -> Interval -> Interval
addInterval = opInterval (+)

mulInterval :: Interval -> Interval -> Interval
mulInterval = opInterval (*)

recipInterval :: Interval -> Interval
recipInterval (Interval l h) | l <= 0 && h >= 0 = recipError
                             | otherwise        = makeInterval (1/h) (1/l)

divInterval :: Interval -> Interval -> Interval
divInterval a b = a `mulInterval` recipInterval b

-- 2.8
subInterval :: Interval -> Interval -> Interval
subInterval = opInterval (-)

-- 2.10, see recipInterval implementation
recipError :: a
recipError = error "Reciprocal of interval spanning 0 is undefined"

makeCenterWidth :: Double -> Double -> Interval
makeCenterWidth c w = makeInterval (c - w) (c + w)

centerInterval :: Interval -> Double
centerInterval (Interval l h) = (l + h) / 2

widthInterval :: Interval -> Double
widthInterval (Interval l h) = (h - l) / 2

-- 2.12
makeCenterPercent :: Double -> Double -> Interval
makeCenterPercent c p = makeCenterWidth c (abs (c * p))

percentInterval :: Interval -> Double
percentInterval a = widthInterval a / centerInterval a

-- 2.17
lastPair :: [a] -> [a]
lastPair l@(_:t) | null t    = l
                 | otherwise = lastPair t
lastPair _ = undefined

-- 2.18
listReverse :: [a] -> [a]
listReverse = go []
  where go !acc (h:t) = go (h:acc) t
        go !acc _     = acc

-- 2.19
usCoins :: (Ord a, Fractional a) => [a]
usCoins = [50, 25, 10, 5, 1]
ukCoins :: (Ord a, Fractional a) => [a]
ukCoins = [100, 50, 20, 10, 5, 2, 1, 0.5]

countChange :: (Ord a, Fractional a) => [a] -> a -> a
countChange cvs@(cv:rest) amount | amount > 0 =
  countChange rest amount + countChange cvs (amount - cv)
countChange _ 0 = 1
countChange _ _ = 0

-- 2.21
squareList1 :: Num a => [a] -> [a]
squareList1 (h:t) = h * h:squareList1 t
squareList1 [] = []

squareList2 :: Num a => [a] -> [a]
squareList2 = map (\x -> x * x)

-- 2.23, basically in the spirit of the scheme for-each. Equivalent to mapM_.
forEach :: (a -> IO b) -> [a] -> IO ()
forEach _ [] = return ()
forEach f (h:t) = f h >> forEach f t

-- 2.28
-- At this point we need to skip a number of exercises and diverge a bit
-- because heterogeneous lists simply don't exist in Haskell.
data Tree a = TBranch [Tree a]
            | TLeaf a
            deriving (Show)

-- h> fringe (TBranch [TBranch [TLeaf 1, TLeaf 2], TBranch [TLeaf 3, TLeaf 4]])
-- [1,2,3,4]
fringe :: Tree a -> [a]
fringe (TLeaf a) = [a]
fringe (TBranch xs) = concatMap fringe xs

-- 2.29
data BMobile = BMobile { bLeftBranch :: BBranch
                       , bRightBranch :: BBranch
                       }
               deriving (Show)
data BBranch = BBranch { bLength :: Double
                       , bStructure :: BStructure
                       }
               deriving (Show)
data BStructure = BSWeight { bsWeight :: Double }
                | BSMobile { bsMobile :: BMobile }
               deriving (Show)

totalWeight :: BMobile -> Double
totalWeight m = branchWeight (bLeftBranch m) + branchWeight (bRightBranch m)
  where branchWeight = structureWeight . bStructure
        structureWeight (BSWeight w) = w
        structureWeight (BSMobile m1) = totalWeight m1

isBalanced :: BMobile -> Bool
isBalanced = maybe False (const True) . go
  where go (BMobile l r) = do
          (lw, ll) <- torque l
          (rw, rl) <- torque r
          guard $ lw * ll == rw * rl
          return $ lw + rw
        torque (BBranch l (BSWeight w)) = return (l, w)
        torque (BBranch l (BSMobile m)) = fmap ((,) l) (go m)

-- 2.30
squareTree1 :: Num a => Tree a -> Tree a
squareTree1 (TLeaf x) = TLeaf $ x * x
squareTree1 (TBranch bs) = TBranch $ go bs
  where go (x:xs) = squareTree1 x:go xs
        go []     = []

squareTree2 :: Num a => Tree a -> Tree a
squareTree2 (TBranch xs) = TBranch $ map squareTree2 xs
squareTree2 (TLeaf x) = TLeaf (x * x)

-- 2.31
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = go
  where go (TLeaf x)    = TLeaf $ f x
        go (TBranch xs) = TBranch $ map go xs

squareTree3 :: Num a => Tree a -> Tree a
squareTree3 = treeMap (\x -> x * x)

-- 2.32
{-
Given s(A) as the set of all subsets of A, and an element b we can define
s(A \/ {b}) as s(A) \/ {A' \/ {b} for all A' in s(A)}.

The subsets function starts with the definition for empty sets s({}) = {{}}
and is recursively evaluated with each element of A to build s(A).
-}
subsets :: [a] -> [[a]]
subsets (h:t) = let rest = subsets t
                in rest ++ map (h:) rest
subsets []    = [[]]

-- 2.33

-- Otherwise known as foldr
accumulate :: (a -> b -> b) -> b -> [a] -> b
accumulate op initial = go
  where go (h:t) = op h $ go t
        go []    = initial

pMap :: (a -> b) -> [a] -> [b]
pMap p xs = accumulate op [] xs
  where op x acc = p x:acc

pAppend :: [a] -> [a] -> [a]
pAppend a b = accumulate (:) b a

pLength :: [a] -> Int
pLength = accumulate op 0
  where op _ acc = 1 + acc

-- 2.34
hornerEval :: Num a => a -> [a] -> a
hornerEval x = accumulate op 0
  where op a acc = a + acc * x

-- 2.35
countLeaves :: Tree a -> Int
countLeaves = length . fringe

-- 2.36
accumulateN :: (a -> b -> b) -> b -> [[a]] -> [b]
accumulateN f initial seqs | null (head seqs) = []
                           | otherwise        =
  accumulate f initial (map head seqs):accumulateN f initial (map tail seqs)

-- 2.37
type Vec a = [a]
type Mat a = [Vec a]
dotProduct :: Num a => Vec a -> Vec a -> a
dotProduct v w = accumulate (+) 0 (zipWith (*) v w)

matrixMulVector :: Num a => Mat a -> Vec a -> Vec a
matrixMulVector m v = map (sum . zipWith (*) v) m

matrixMulMatrix :: Num a => Mat a -> Mat a -> Mat a
matrixMulMatrix m n = let cols = matrixTranspose n
                      in map (matrixMulVector cols) m

matrixTranspose :: Mat a -> Mat a
matrixTranspose = accumulateN (:) []

-- 2.39
reverseR :: [a] -> [a]
reverseR = foldr (\x acc -> acc ++ [x]) []

reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

-- 2.40
uniquePairs :: Integral a => a -> [(a, a)]
uniquePairs n = concatMap (\j -> map ((,) j) [j+1..n]) [1..n-1]

primesTo :: Integral a => a -> [a]
primesTo m | m < 2     = []
           | otherwise = 2 : eratos [3,5..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..m])
   minus (x:xs) (y:ys) = case compare x y of
     LT -> x : minus  xs  (y:ys)
     EQ ->     minus  xs     ys
     GT ->     minus (x:xs)  ys
   minus  xs     _     = xs

primeSumPairs :: Integer -> [(Integer, Integer)]
primeSumPairs n = filter f (uniquePairs n)
  where f = flip elem primes . uncurry (+)
        primes = primesTo n

-- 2.41
orderedTriples :: Integral a => a -> a -> [(a, a, a)]
orderedTriples n s = [(i,j,k)
                     | i <- [1..n-2],
                       j <- [i+1..n-1],
                       k <- [i+2..n],
                       i + j + k == s]
