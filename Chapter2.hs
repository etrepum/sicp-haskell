{-# LANGUAGE BangPatterns #-}
import qualified Data.Ratio as R

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