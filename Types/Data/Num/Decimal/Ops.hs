-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Num.Decimal.Ops
-- Copyright   :  (c) 2008 Peter Gavin
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  pgavin@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, requires ghc >= 6.9)
--
-- Type-level numerical operations using type families.
-- 
----------------------------------------------------------------------------

module Types.Data.Num.Decimal.Ops ()
    where

import Types.Base
import Types.Data.Bool
import Types.Data.Ord
import Types.Data.Num.Ops
import Types.Data.Num.Decimal.Literals
import Types.Data.Num.Decimal.Digits

instance IntegerR Decimal where
    reifyIntegral _ i k = reifyIntegral' i (\(_ :: s) -> k (undefined :: Dec s))

reifyIntegral' :: Integer -> (forall s. IntegerT' s => s -> w) -> w
reifyIntegral' i k | i <  0    = go (negate i) (\(_ :: s) -> k (undefined :: Neg' s)) 
                   | otherwise = go i k
  where
   go :: Integer -> (forall s. IntegerT' s => s -> w) -> w
   go 0 k = k (undefined :: DecN)
   go i k = let (j, d) =  quotRem i 10 in case d of
     0 -> go j (\(_ :: s) -> k (undefined :: s :. Dec0))
     1 -> go j (\(_ :: s) -> k (undefined :: s :. Dec1))
     2 -> go j (\(_ :: s) -> k (undefined :: s :. Dec2))
     3 -> go j (\(_ :: s) -> k (undefined :: s :. Dec3))
     4 -> go j (\(_ :: s) -> k (undefined :: s :. Dec4))
     5 -> go j (\(_ :: s) -> k (undefined :: s :. Dec5))
     6 -> go j (\(_ :: s) -> k (undefined :: s :. Dec6))
     7 -> go j (\(_ :: s) -> k (undefined :: s :. Dec7))
     8 -> go j (\(_ :: s) -> k (undefined :: s :. Dec8))
     9 -> go j (\(_ :: s) -> k (undefined :: s :. Dec9))

instance IntegerT' x => IntegerT (Dec x) where
    fromIntegerT _ = fromIntegerT' (undefined :: x)
    type Repr (Dec x) = Decimal

class IntegerT' x where
    fromIntegerT' :: Num y => x -> y
instance IntegerT' DecN where
    fromIntegerT' _ = 0
instance IntegerT' x => IntegerT' (Neg' x) where
    fromIntegerT' _ = negate (fromIntegerT' (undefined :: x))
instance IntegerT' xh => IntegerT' (xh :. Dec0) where
    fromIntegerT' _ = 0 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec1) where
    fromIntegerT' _ = 1 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec2) where
    fromIntegerT' _ = 2 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec3) where
    fromIntegerT' _ = 3 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec4) where
    fromIntegerT' _ = 4 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec5) where
    fromIntegerT' _ = 5 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec6) where
    fromIntegerT' _ = 6 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec7) where
    fromIntegerT' _ = 7 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec8) where
    fromIntegerT' _ = 8 + 10 * fromIntegerT' (undefined :: xh)
instance IntegerT' xh => IntegerT' (xh :. Dec9) where
    fromIntegerT' _ = 9 + 10 * fromIntegerT' (undefined :: xh)

type family Normalize x
type instance Normalize (Dec x) = Dec (Normalize' x)

type family Normalize' x
type instance Normalize' DecN       = DecN
type instance Normalize' (xh :. xl) = NormalizePos (xh :. xl)
type instance Normalize' (Neg' x)   = NormalizeNeg x

type family NormalizePos x
type instance NormalizePos x = NormalizePos' (ReverseDigits x)
type family NormalizePos' x
type instance NormalizePos' DecN         = DecN
type instance NormalizePos' (xh :. Dec0) = NormalizePos' xh
type instance NormalizePos' (xh :. Dec1) = ReverseDigits (xh :. Dec1)
type instance NormalizePos' (xh :. Dec2) = ReverseDigits (xh :. Dec2)
type instance NormalizePos' (xh :. Dec3) = ReverseDigits (xh :. Dec3)
type instance NormalizePos' (xh :. Dec4) = ReverseDigits (xh :. Dec4)
type instance NormalizePos' (xh :. Dec5) = ReverseDigits (xh :. Dec5)
type instance NormalizePos' (xh :. Dec6) = ReverseDigits (xh :. Dec6)
type instance NormalizePos' (xh :. Dec7) = ReverseDigits (xh :. Dec7)
type instance NormalizePos' (xh :. Dec8) = ReverseDigits (xh :. Dec8)
type instance NormalizePos' (xh :. Dec9) = ReverseDigits (xh :. Dec9)

type family ReverseDigits x
type instance ReverseDigits DecN       = DecN
type instance ReverseDigits (xh :. xl) = ReverseDigits' (xh :. xl) DecN

type family ReverseDigits' x y
type instance ReverseDigits' DecN y = y
type instance ReverseDigits' (xh :. xl) y = ReverseDigits' xh (y :. xl)

type family NormalizeNeg x
type instance NormalizeNeg (Neg' x)   = Normalize' x -- negate . negate == id
type instance NormalizeNeg DecN       = DecN         -- negate 0 = 0
type instance NormalizeNeg (xh :. xl) = NormalizeNeg' (NormalizePos (xh :. xl))

type family NormalizeNeg' x
type instance NormalizeNeg' DecN = DecN
type instance NormalizeNeg' (xh :. xl) = Neg' (xh :. xl)

-- type family IsPositive x
type instance IsPositive (Dec x) = IsPositive' x
type family IsPositive' x
type instance IsPositive' (Neg' x)   = False
type instance IsPositive' DecN       = False
type instance IsPositive' (xh :. xl) = True

-- type family IsZero x
type instance IsZero (Dec x) = IsZero' x
type family IsZero' x
type instance IsZero' (Neg' x)   = False
type instance IsZero' DecN       = True
type instance IsZero' (xh :. xl) = False

-- type family IsNegative x
type instance IsNegative (Dec x) = IsNegative' x
type family IsNegative' x
type instance IsNegative' (Neg' x)   = True
type instance IsNegative' DecN       = False
type instance IsNegative' (xh :. xl) = False

-- type family IsNatural x
type instance IsNatural (Dec x) = IsNatural' x
type family IsNatural' x
type instance IsNatural' (Neg' x)   = False
type instance IsNatural' DecN       = True
type instance IsNatural' (xh :. xl) = True

-- type family Neg x
type instance Neg (Dec DecN)     = Dec DecN
type instance Neg (Dec (Neg' x)) = Dec x
type instance Neg (Dec (xh :. xl)) = Dec (Neg' (xh :. xl))

-- type family Succ x
type instance Succ (Dec x) = Dec (Succ' x)

type family Succ' x
type instance Succ' (Neg' x   ) = Normalize' (Neg' (Pred'' x))
type instance Succ' (DecN     ) = DecN :. Dec1
type instance Succ' (x :. Dec0) = x :. Dec1
type instance Succ' (x :. Dec1) = x :. Dec2
type instance Succ' (x :. Dec2) = x :. Dec3
type instance Succ' (x :. Dec3) = x :. Dec4
type instance Succ' (x :. Dec4) = x :. Dec5
type instance Succ' (x :. Dec5) = x :. Dec6
type instance Succ' (x :. Dec6) = x :. Dec7
type instance Succ' (x :. Dec7) = x :. Dec8
type instance Succ' (x :. Dec8) = x :. Dec9
type instance Succ' (x :. Dec9) = Succ' x :. Dec0

-- type family Pred x
type instance Pred (Dec x)     = Dec (Pred' x)

type family Pred' x
type instance Pred' x = Normalize' (Pred'' x)
type family Pred'' x
type instance Pred'' (Neg' x   ) = Neg' (Succ' x)
type instance Pred'' (DecN     ) = Neg' (DecN :. Dec1)
type instance Pred'' (x :. Dec0) = Pred'' x :. Dec9
type instance Pred'' (x :. Dec1) = x :. Dec0
type instance Pred'' (x :. Dec2) = x :. Dec1
type instance Pred'' (x :. Dec3) = x :. Dec2
type instance Pred'' (x :. Dec4) = x :. Dec3
type instance Pred'' (x :. Dec5) = x :. Dec4
type instance Pred'' (x :. Dec6) = x :. Dec5
type instance Pred'' (x :. Dec7) = x :. Dec6
type instance Pred'' (x :. Dec8) = x :. Dec7
type instance Pred'' (x :. Dec9) = x :. Dec8

--------------------
-- Addition

type family AddDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance AddDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x+y) `mod` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance AddDigit Dec0 Dec0 = Dec0
type instance AddDigit Dec0 Dec1 = Dec1
type instance AddDigit Dec0 Dec2 = Dec2
type instance AddDigit Dec0 Dec3 = Dec3
type instance AddDigit Dec0 Dec4 = Dec4
type instance AddDigit Dec0 Dec5 = Dec5
type instance AddDigit Dec0 Dec6 = Dec6
type instance AddDigit Dec0 Dec7 = Dec7
type instance AddDigit Dec0 Dec8 = Dec8
type instance AddDigit Dec0 Dec9 = Dec9

type instance AddDigit Dec1 Dec0 = Dec1
type instance AddDigit Dec1 Dec1 = Dec2
type instance AddDigit Dec1 Dec2 = Dec3
type instance AddDigit Dec1 Dec3 = Dec4
type instance AddDigit Dec1 Dec4 = Dec5
type instance AddDigit Dec1 Dec5 = Dec6
type instance AddDigit Dec1 Dec6 = Dec7
type instance AddDigit Dec1 Dec7 = Dec8
type instance AddDigit Dec1 Dec8 = Dec9
type instance AddDigit Dec1 Dec9 = Dec0

type instance AddDigit Dec2 Dec0 = Dec2
type instance AddDigit Dec2 Dec1 = Dec3
type instance AddDigit Dec2 Dec2 = Dec4
type instance AddDigit Dec2 Dec3 = Dec5
type instance AddDigit Dec2 Dec4 = Dec6
type instance AddDigit Dec2 Dec5 = Dec7
type instance AddDigit Dec2 Dec6 = Dec8
type instance AddDigit Dec2 Dec7 = Dec9
type instance AddDigit Dec2 Dec8 = Dec0
type instance AddDigit Dec2 Dec9 = Dec1

type instance AddDigit Dec3 Dec0 = Dec3
type instance AddDigit Dec3 Dec1 = Dec4
type instance AddDigit Dec3 Dec2 = Dec5
type instance AddDigit Dec3 Dec3 = Dec6
type instance AddDigit Dec3 Dec4 = Dec7
type instance AddDigit Dec3 Dec5 = Dec8
type instance AddDigit Dec3 Dec6 = Dec9
type instance AddDigit Dec3 Dec7 = Dec0
type instance AddDigit Dec3 Dec8 = Dec1
type instance AddDigit Dec3 Dec9 = Dec2

type instance AddDigit Dec4 Dec0 = Dec4
type instance AddDigit Dec4 Dec1 = Dec5
type instance AddDigit Dec4 Dec2 = Dec6
type instance AddDigit Dec4 Dec3 = Dec7
type instance AddDigit Dec4 Dec4 = Dec8
type instance AddDigit Dec4 Dec5 = Dec9
type instance AddDigit Dec4 Dec6 = Dec0
type instance AddDigit Dec4 Dec7 = Dec1
type instance AddDigit Dec4 Dec8 = Dec2
type instance AddDigit Dec4 Dec9 = Dec3

type instance AddDigit Dec5 Dec0 = Dec5
type instance AddDigit Dec5 Dec1 = Dec6
type instance AddDigit Dec5 Dec2 = Dec7
type instance AddDigit Dec5 Dec3 = Dec8
type instance AddDigit Dec5 Dec4 = Dec9
type instance AddDigit Dec5 Dec5 = Dec0
type instance AddDigit Dec5 Dec6 = Dec1
type instance AddDigit Dec5 Dec7 = Dec2
type instance AddDigit Dec5 Dec8 = Dec3
type instance AddDigit Dec5 Dec9 = Dec4

type instance AddDigit Dec6 Dec0 = Dec6
type instance AddDigit Dec6 Dec1 = Dec7
type instance AddDigit Dec6 Dec2 = Dec8
type instance AddDigit Dec6 Dec3 = Dec9
type instance AddDigit Dec6 Dec4 = Dec0
type instance AddDigit Dec6 Dec5 = Dec1
type instance AddDigit Dec6 Dec6 = Dec2
type instance AddDigit Dec6 Dec7 = Dec3
type instance AddDigit Dec6 Dec8 = Dec4
type instance AddDigit Dec6 Dec9 = Dec5

type instance AddDigit Dec7 Dec0 = Dec7
type instance AddDigit Dec7 Dec1 = Dec8
type instance AddDigit Dec7 Dec2 = Dec9
type instance AddDigit Dec7 Dec3 = Dec0
type instance AddDigit Dec7 Dec4 = Dec1
type instance AddDigit Dec7 Dec5 = Dec2
type instance AddDigit Dec7 Dec6 = Dec3
type instance AddDigit Dec7 Dec7 = Dec4
type instance AddDigit Dec7 Dec8 = Dec5
type instance AddDigit Dec7 Dec9 = Dec6

type instance AddDigit Dec8 Dec0 = Dec8
type instance AddDigit Dec8 Dec1 = Dec9
type instance AddDigit Dec8 Dec2 = Dec0
type instance AddDigit Dec8 Dec3 = Dec1
type instance AddDigit Dec8 Dec4 = Dec2
type instance AddDigit Dec8 Dec5 = Dec3
type instance AddDigit Dec8 Dec6 = Dec4
type instance AddDigit Dec8 Dec7 = Dec5
type instance AddDigit Dec8 Dec8 = Dec6
type instance AddDigit Dec8 Dec9 = Dec7

type instance AddDigit Dec9 Dec0 = Dec9
type instance AddDigit Dec9 Dec1 = Dec0
type instance AddDigit Dec9 Dec2 = Dec1
type instance AddDigit Dec9 Dec3 = Dec2
type instance AddDigit Dec9 Dec4 = Dec3
type instance AddDigit Dec9 Dec5 = Dec4
type instance AddDigit Dec9 Dec6 = Dec5
type instance AddDigit Dec9 Dec7 = Dec6
type instance AddDigit Dec9 Dec8 = Dec7
type instance AddDigit Dec9 Dec9 = Dec8

-- | If adding @x@ and @y@ would not carry, then
--   @AddCarry x y z@ evaluates to @z@.  Otherwise,
--   @AddCarry x y z@ evaluates to @Succ' z@
type family AddCarry x y z
-- putStr $ unlines $ concat $ [ [ "type instance AddCarry Dec" ++ show x ++ " Dec" ++ show y ++ " x = " ++ (if x + y >= 10 then "Succ'" else "Id") ++ " x" | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance AddCarry Dec0 Dec0 x = Id x
type instance AddCarry Dec0 Dec1 x = Id x
type instance AddCarry Dec0 Dec2 x = Id x
type instance AddCarry Dec0 Dec3 x = Id x
type instance AddCarry Dec0 Dec4 x = Id x
type instance AddCarry Dec0 Dec5 x = Id x
type instance AddCarry Dec0 Dec6 x = Id x
type instance AddCarry Dec0 Dec7 x = Id x
type instance AddCarry Dec0 Dec8 x = Id x
type instance AddCarry Dec0 Dec9 x = Id x

type instance AddCarry Dec1 Dec0 x = Id x
type instance AddCarry Dec1 Dec1 x = Id x
type instance AddCarry Dec1 Dec2 x = Id x
type instance AddCarry Dec1 Dec3 x = Id x
type instance AddCarry Dec1 Dec4 x = Id x
type instance AddCarry Dec1 Dec5 x = Id x
type instance AddCarry Dec1 Dec6 x = Id x
type instance AddCarry Dec1 Dec7 x = Id x
type instance AddCarry Dec1 Dec8 x = Id x
type instance AddCarry Dec1 Dec9 x = Succ' x

type instance AddCarry Dec2 Dec0 x = Id x
type instance AddCarry Dec2 Dec1 x = Id x
type instance AddCarry Dec2 Dec2 x = Id x
type instance AddCarry Dec2 Dec3 x = Id x
type instance AddCarry Dec2 Dec4 x = Id x
type instance AddCarry Dec2 Dec5 x = Id x
type instance AddCarry Dec2 Dec6 x = Id x
type instance AddCarry Dec2 Dec7 x = Id x
type instance AddCarry Dec2 Dec8 x = Succ' x
type instance AddCarry Dec2 Dec9 x = Succ' x

type instance AddCarry Dec3 Dec0 x = Id x
type instance AddCarry Dec3 Dec1 x = Id x
type instance AddCarry Dec3 Dec2 x = Id x
type instance AddCarry Dec3 Dec3 x = Id x
type instance AddCarry Dec3 Dec4 x = Id x
type instance AddCarry Dec3 Dec5 x = Id x
type instance AddCarry Dec3 Dec6 x = Id x
type instance AddCarry Dec3 Dec7 x = Succ' x
type instance AddCarry Dec3 Dec8 x = Succ' x
type instance AddCarry Dec3 Dec9 x = Succ' x

type instance AddCarry Dec4 Dec0 x = Id x
type instance AddCarry Dec4 Dec1 x = Id x
type instance AddCarry Dec4 Dec2 x = Id x
type instance AddCarry Dec4 Dec3 x = Id x
type instance AddCarry Dec4 Dec4 x = Id x
type instance AddCarry Dec4 Dec5 x = Id x
type instance AddCarry Dec4 Dec6 x = Succ' x
type instance AddCarry Dec4 Dec7 x = Succ' x
type instance AddCarry Dec4 Dec8 x = Succ' x
type instance AddCarry Dec4 Dec9 x = Succ' x

type instance AddCarry Dec5 Dec0 x = Id x
type instance AddCarry Dec5 Dec1 x = Id x
type instance AddCarry Dec5 Dec2 x = Id x
type instance AddCarry Dec5 Dec3 x = Id x
type instance AddCarry Dec5 Dec4 x = Id x
type instance AddCarry Dec5 Dec5 x = Succ' x
type instance AddCarry Dec5 Dec6 x = Succ' x
type instance AddCarry Dec5 Dec7 x = Succ' x
type instance AddCarry Dec5 Dec8 x = Succ' x
type instance AddCarry Dec5 Dec9 x = Succ' x

type instance AddCarry Dec6 Dec0 x = Id x
type instance AddCarry Dec6 Dec1 x = Id x
type instance AddCarry Dec6 Dec2 x = Id x
type instance AddCarry Dec6 Dec3 x = Id x
type instance AddCarry Dec6 Dec4 x = Succ' x
type instance AddCarry Dec6 Dec5 x = Succ' x
type instance AddCarry Dec6 Dec6 x = Succ' x
type instance AddCarry Dec6 Dec7 x = Succ' x
type instance AddCarry Dec6 Dec8 x = Succ' x
type instance AddCarry Dec6 Dec9 x = Succ' x

type instance AddCarry Dec7 Dec0 x = Id x
type instance AddCarry Dec7 Dec1 x = Id x
type instance AddCarry Dec7 Dec2 x = Id x
type instance AddCarry Dec7 Dec3 x = Succ' x
type instance AddCarry Dec7 Dec4 x = Succ' x
type instance AddCarry Dec7 Dec5 x = Succ' x
type instance AddCarry Dec7 Dec6 x = Succ' x
type instance AddCarry Dec7 Dec7 x = Succ' x
type instance AddCarry Dec7 Dec8 x = Succ' x
type instance AddCarry Dec7 Dec9 x = Succ' x

type instance AddCarry Dec8 Dec0 x = Id x
type instance AddCarry Dec8 Dec1 x = Id x
type instance AddCarry Dec8 Dec2 x = Succ' x
type instance AddCarry Dec8 Dec3 x = Succ' x
type instance AddCarry Dec8 Dec4 x = Succ' x
type instance AddCarry Dec8 Dec5 x = Succ' x
type instance AddCarry Dec8 Dec6 x = Succ' x
type instance AddCarry Dec8 Dec7 x = Succ' x
type instance AddCarry Dec8 Dec8 x = Succ' x
type instance AddCarry Dec8 Dec9 x = Succ' x

type instance AddCarry Dec9 Dec0 x = Id x
type instance AddCarry Dec9 Dec1 x = Succ' x
type instance AddCarry Dec9 Dec2 x = Succ' x
type instance AddCarry Dec9 Dec3 x = Succ' x
type instance AddCarry Dec9 Dec4 x = Succ' x
type instance AddCarry Dec9 Dec5 x = Succ' x
type instance AddCarry Dec9 Dec6 x = Succ' x
type instance AddCarry Dec9 Dec7 x = Succ' x
type instance AddCarry Dec9 Dec8 x = Succ' x
type instance AddCarry Dec9 Dec9 x = Succ' x

-- type family x :+: y
type instance Dec x :+: Dec y = Dec (Add x y)

type family Add x y
type instance Add (DecN    ) (DecN    ) = DecN
type instance Add (xh :. xl) (DecN    ) = (xh :. xl)
type instance Add (DecN    ) (yh :. yl) = (yh :. yl)
type instance Add (Neg' x  ) (Neg' y  ) = Neg' (Add x y)
type instance Add (xh :. xl) (Neg' y  ) = Sub (xh :. xl) y
type instance Add (Neg' x  ) (yh :. yl) = Sub (yh :. yl) x
type instance Add (xh :. xl) (yh :. yl) = (AddCarry xl yl (Add xh yh)) :. (AddDigit xl yl)

--------------------
-- Subtraction

type family SubDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance SubDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = Dec" ++ show ((x-y) `mod` 10) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance SubDigit Dec0 Dec0 = Dec0
type instance SubDigit Dec0 Dec1 = Dec9
type instance SubDigit Dec0 Dec2 = Dec8
type instance SubDigit Dec0 Dec3 = Dec7
type instance SubDigit Dec0 Dec4 = Dec6
type instance SubDigit Dec0 Dec5 = Dec5
type instance SubDigit Dec0 Dec6 = Dec4
type instance SubDigit Dec0 Dec7 = Dec3
type instance SubDigit Dec0 Dec8 = Dec2
type instance SubDigit Dec0 Dec9 = Dec1

type instance SubDigit Dec1 Dec0 = Dec1
type instance SubDigit Dec1 Dec1 = Dec0
type instance SubDigit Dec1 Dec2 = Dec9
type instance SubDigit Dec1 Dec3 = Dec8
type instance SubDigit Dec1 Dec4 = Dec7
type instance SubDigit Dec1 Dec5 = Dec6
type instance SubDigit Dec1 Dec6 = Dec5
type instance SubDigit Dec1 Dec7 = Dec4
type instance SubDigit Dec1 Dec8 = Dec3
type instance SubDigit Dec1 Dec9 = Dec2

type instance SubDigit Dec2 Dec0 = Dec2
type instance SubDigit Dec2 Dec1 = Dec1
type instance SubDigit Dec2 Dec2 = Dec0
type instance SubDigit Dec2 Dec3 = Dec9
type instance SubDigit Dec2 Dec4 = Dec8
type instance SubDigit Dec2 Dec5 = Dec7
type instance SubDigit Dec2 Dec6 = Dec6
type instance SubDigit Dec2 Dec7 = Dec5
type instance SubDigit Dec2 Dec8 = Dec4
type instance SubDigit Dec2 Dec9 = Dec3

type instance SubDigit Dec3 Dec0 = Dec3
type instance SubDigit Dec3 Dec1 = Dec2
type instance SubDigit Dec3 Dec2 = Dec1
type instance SubDigit Dec3 Dec3 = Dec0
type instance SubDigit Dec3 Dec4 = Dec9
type instance SubDigit Dec3 Dec5 = Dec8
type instance SubDigit Dec3 Dec6 = Dec7
type instance SubDigit Dec3 Dec7 = Dec6
type instance SubDigit Dec3 Dec8 = Dec5
type instance SubDigit Dec3 Dec9 = Dec4

type instance SubDigit Dec4 Dec0 = Dec4
type instance SubDigit Dec4 Dec1 = Dec3
type instance SubDigit Dec4 Dec2 = Dec2
type instance SubDigit Dec4 Dec3 = Dec1
type instance SubDigit Dec4 Dec4 = Dec0
type instance SubDigit Dec4 Dec5 = Dec9
type instance SubDigit Dec4 Dec6 = Dec8
type instance SubDigit Dec4 Dec7 = Dec7
type instance SubDigit Dec4 Dec8 = Dec6
type instance SubDigit Dec4 Dec9 = Dec5

type instance SubDigit Dec5 Dec0 = Dec5
type instance SubDigit Dec5 Dec1 = Dec4
type instance SubDigit Dec5 Dec2 = Dec3
type instance SubDigit Dec5 Dec3 = Dec2
type instance SubDigit Dec5 Dec4 = Dec1
type instance SubDigit Dec5 Dec5 = Dec0
type instance SubDigit Dec5 Dec6 = Dec9
type instance SubDigit Dec5 Dec7 = Dec8
type instance SubDigit Dec5 Dec8 = Dec7
type instance SubDigit Dec5 Dec9 = Dec6

type instance SubDigit Dec6 Dec0 = Dec6
type instance SubDigit Dec6 Dec1 = Dec5
type instance SubDigit Dec6 Dec2 = Dec4
type instance SubDigit Dec6 Dec3 = Dec3
type instance SubDigit Dec6 Dec4 = Dec2
type instance SubDigit Dec6 Dec5 = Dec1
type instance SubDigit Dec6 Dec6 = Dec0
type instance SubDigit Dec6 Dec7 = Dec9
type instance SubDigit Dec6 Dec8 = Dec8
type instance SubDigit Dec6 Dec9 = Dec7

type instance SubDigit Dec7 Dec0 = Dec7
type instance SubDigit Dec7 Dec1 = Dec6
type instance SubDigit Dec7 Dec2 = Dec5
type instance SubDigit Dec7 Dec3 = Dec4
type instance SubDigit Dec7 Dec4 = Dec3
type instance SubDigit Dec7 Dec5 = Dec2
type instance SubDigit Dec7 Dec6 = Dec1
type instance SubDigit Dec7 Dec7 = Dec0
type instance SubDigit Dec7 Dec8 = Dec9
type instance SubDigit Dec7 Dec9 = Dec8

type instance SubDigit Dec8 Dec0 = Dec8
type instance SubDigit Dec8 Dec1 = Dec7
type instance SubDigit Dec8 Dec2 = Dec6
type instance SubDigit Dec8 Dec3 = Dec5
type instance SubDigit Dec8 Dec4 = Dec4
type instance SubDigit Dec8 Dec5 = Dec3
type instance SubDigit Dec8 Dec6 = Dec2
type instance SubDigit Dec8 Dec7 = Dec1
type instance SubDigit Dec8 Dec8 = Dec0
type instance SubDigit Dec8 Dec9 = Dec9

type instance SubDigit Dec9 Dec0 = Dec9
type instance SubDigit Dec9 Dec1 = Dec8
type instance SubDigit Dec9 Dec2 = Dec7
type instance SubDigit Dec9 Dec3 = Dec6
type instance SubDigit Dec9 Dec4 = Dec5
type instance SubDigit Dec9 Dec5 = Dec4
type instance SubDigit Dec9 Dec6 = Dec3
type instance SubDigit Dec9 Dec7 = Dec2
type instance SubDigit Dec9 Dec8 = Dec1
type instance SubDigit Dec9 Dec9 = Dec0

-- | If subtracting @y@ from @x@ would not borrow, then
--   @Borrow x y z@ evaluates to @z@.  Otherwise,
--   @Borrow x y z@ evaluates to @Pred' z@
type family Borrow x y z
-- putStr $ unlines $ concat $ [ [ "type instance Borrow Dec" ++ show x ++ " Dec" ++ show y ++ " x = " ++ (if x < y then "Pred'" else "Id") ++ " x" | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance Borrow Dec0 Dec0 x = Id x
type instance Borrow Dec0 Dec1 x = Pred' x
type instance Borrow Dec0 Dec2 x = Pred' x
type instance Borrow Dec0 Dec3 x = Pred' x
type instance Borrow Dec0 Dec4 x = Pred' x
type instance Borrow Dec0 Dec5 x = Pred' x
type instance Borrow Dec0 Dec6 x = Pred' x
type instance Borrow Dec0 Dec7 x = Pred' x
type instance Borrow Dec0 Dec8 x = Pred' x
type instance Borrow Dec0 Dec9 x = Pred' x

type instance Borrow Dec1 Dec0 x = Id x
type instance Borrow Dec1 Dec1 x = Id x
type instance Borrow Dec1 Dec2 x = Pred' x
type instance Borrow Dec1 Dec3 x = Pred' x
type instance Borrow Dec1 Dec4 x = Pred' x
type instance Borrow Dec1 Dec5 x = Pred' x
type instance Borrow Dec1 Dec6 x = Pred' x
type instance Borrow Dec1 Dec7 x = Pred' x
type instance Borrow Dec1 Dec8 x = Pred' x
type instance Borrow Dec1 Dec9 x = Pred' x

type instance Borrow Dec2 Dec0 x = Id x
type instance Borrow Dec2 Dec1 x = Id x
type instance Borrow Dec2 Dec2 x = Id x
type instance Borrow Dec2 Dec3 x = Pred' x
type instance Borrow Dec2 Dec4 x = Pred' x
type instance Borrow Dec2 Dec5 x = Pred' x
type instance Borrow Dec2 Dec6 x = Pred' x
type instance Borrow Dec2 Dec7 x = Pred' x
type instance Borrow Dec2 Dec8 x = Pred' x
type instance Borrow Dec2 Dec9 x = Pred' x

type instance Borrow Dec3 Dec0 x = Id x
type instance Borrow Dec3 Dec1 x = Id x
type instance Borrow Dec3 Dec2 x = Id x
type instance Borrow Dec3 Dec3 x = Id x
type instance Borrow Dec3 Dec4 x = Pred' x
type instance Borrow Dec3 Dec5 x = Pred' x
type instance Borrow Dec3 Dec6 x = Pred' x
type instance Borrow Dec3 Dec7 x = Pred' x
type instance Borrow Dec3 Dec8 x = Pred' x
type instance Borrow Dec3 Dec9 x = Pred' x

type instance Borrow Dec4 Dec0 x = Id x
type instance Borrow Dec4 Dec1 x = Id x
type instance Borrow Dec4 Dec2 x = Id x
type instance Borrow Dec4 Dec3 x = Id x
type instance Borrow Dec4 Dec4 x = Id x
type instance Borrow Dec4 Dec5 x = Pred' x
type instance Borrow Dec4 Dec6 x = Pred' x
type instance Borrow Dec4 Dec7 x = Pred' x
type instance Borrow Dec4 Dec8 x = Pred' x
type instance Borrow Dec4 Dec9 x = Pred' x

type instance Borrow Dec5 Dec0 x = Id x
type instance Borrow Dec5 Dec1 x = Id x
type instance Borrow Dec5 Dec2 x = Id x
type instance Borrow Dec5 Dec3 x = Id x
type instance Borrow Dec5 Dec4 x = Id x
type instance Borrow Dec5 Dec5 x = Id x
type instance Borrow Dec5 Dec6 x = Pred' x
type instance Borrow Dec5 Dec7 x = Pred' x
type instance Borrow Dec5 Dec8 x = Pred' x
type instance Borrow Dec5 Dec9 x = Pred' x

type instance Borrow Dec6 Dec0 x = Id x
type instance Borrow Dec6 Dec1 x = Id x
type instance Borrow Dec6 Dec2 x = Id x
type instance Borrow Dec6 Dec3 x = Id x
type instance Borrow Dec6 Dec4 x = Id x
type instance Borrow Dec6 Dec5 x = Id x
type instance Borrow Dec6 Dec6 x = Id x
type instance Borrow Dec6 Dec7 x = Pred' x
type instance Borrow Dec6 Dec8 x = Pred' x
type instance Borrow Dec6 Dec9 x = Pred' x

type instance Borrow Dec7 Dec0 x = Id x
type instance Borrow Dec7 Dec1 x = Id x
type instance Borrow Dec7 Dec2 x = Id x
type instance Borrow Dec7 Dec3 x = Id x
type instance Borrow Dec7 Dec4 x = Id x
type instance Borrow Dec7 Dec5 x = Id x
type instance Borrow Dec7 Dec6 x = Id x
type instance Borrow Dec7 Dec7 x = Id x
type instance Borrow Dec7 Dec8 x = Pred' x
type instance Borrow Dec7 Dec9 x = Pred' x

type instance Borrow Dec8 Dec0 x = Id x
type instance Borrow Dec8 Dec1 x = Id x
type instance Borrow Dec8 Dec2 x = Id x
type instance Borrow Dec8 Dec3 x = Id x
type instance Borrow Dec8 Dec4 x = Id x
type instance Borrow Dec8 Dec5 x = Id x
type instance Borrow Dec8 Dec6 x = Id x
type instance Borrow Dec8 Dec7 x = Id x
type instance Borrow Dec8 Dec8 x = Id x
type instance Borrow Dec8 Dec9 x = Pred' x

type instance Borrow Dec9 Dec0 x = Id x
type instance Borrow Dec9 Dec1 x = Id x
type instance Borrow Dec9 Dec2 x = Id x
type instance Borrow Dec9 Dec3 x = Id x
type instance Borrow Dec9 Dec4 x = Id x
type instance Borrow Dec9 Dec5 x = Id x
type instance Borrow Dec9 Dec6 x = Id x
type instance Borrow Dec9 Dec7 x = Id x
type instance Borrow Dec9 Dec8 x = Id x
type instance Borrow Dec9 Dec9 x = Id x

-- type family x :-: y
type instance Dec x :-: Dec y = Dec (Sub x y)

type family Sub x y
type instance Sub x y = Normalize' (Sub' x y)

type family Sub' x y
type instance Sub' (Neg' x  ) (Neg' y  ) = Sub' y x
type instance Sub' (Neg' x  ) (DecN    ) = Neg' x
type instance Sub' (Neg' x  ) (yh :. yl) = Neg' (Add x (yh :. yl))
type instance Sub' (DecN    ) (Neg' x  ) = x
type instance Sub' (DecN    ) (DecN    ) = DecN
type instance Sub' (DecN    ) (yh :. yl) = Neg' (yh :. yl)
type instance Sub' (xh :. xl) (Neg' y  ) = Add (xh :. xl) y
type instance Sub' (xh :. xl) (DecN    ) = xh :. xl
type instance Sub' (xh :. xl) (yh :. yl) = Sub'' (xh :. xl) (yh :. yl) (Compare' (xh :. xl) (yh :. yl))

type family Sub'' x y c
type instance Sub'' x y GT = SubPos x y DecN
type instance Sub'' x y EQ = DecN
type instance Sub'' x y LT = Neg' (SubPos y x DecN)

type family SubPos x y z
type instance SubPos (xh :. xl) (yh :. yl) z = SubPos (Borrow xl yl xh) yh (z :. SubDigit xl yl)
type instance SubPos (xh :. xl) DecN       z = SubPos xh DecN (z :. xl)
type instance SubPos DecN       DecN       z = ReverseDigits z

--------------------
-- Multiplication

-- type family Mul2 x
type instance Mul2 (Dec x) = Mul2' x
type family Mul2' x
type instance Mul2' x = Add x x

-- type family x :*: y
type instance (Dec x) :*: (Dec y) = Dec (Mul x y)

-- Peasant style
type family Mul x y
type instance Mul DecN       DecN       = DecN            -- 0 * 0 = 0
type instance Mul (xh :. xl) DecN       = DecN            -- x * 0 = 0
type instance Mul DecN       (yh :. yl) = DecN            -- 0 * x = 0
type instance Mul (Neg' x)   (Neg' y)   = Mul x y        -- -x * -y = x*y
type instance Mul (xh :. xl) (Neg' y)   = Neg' (Mul (xh :. xl) y)  -- x * -y = -(x*y)
type instance Mul (Neg' x)   (yh :. yl) = Neg' (Mul x (yh :. yl))  -- -x * y = -(x*y)
type instance Mul (xh :. xl) (yh :. yl) = Mul' (xh :. xl) (yh :. yl) DecN -- x & y positive

type family Mul' x y z
type instance Mul' x DecN       z = z
type instance Mul' x (yh :. yl) z = Mul' (Mul2' x) (Div2' (yh :. yl)) (If (IsEven' (yh :. yl)) z (Add z x))

-- type family Fac x
type instance Fac x = Fac' x (IsZero x)
type family Fac' x is0
type instance Fac' x True = D1
type instance Fac' x False = x :*: Fac (Pred x)

-----------
-- Division / Modulus

-- type family IsEven x
type instance IsEven (Dec x) = IsEven' x
type family IsEven' x
type instance IsEven' DecN = True
type instance IsEven' (Neg' x) = IsEven' x
type instance IsEven' (xh :. Dec0) = True
type instance IsEven' (xh :. Dec1) = False
type instance IsEven' (xh :. Dec2) = True
type instance IsEven' (xh :. Dec3) = False
type instance IsEven' (xh :. Dec4) = True
type instance IsEven' (xh :. Dec5) = False
type instance IsEven' (xh :. Dec6) = True
type instance IsEven' (xh :. Dec7) = False
type instance IsEven' (xh :. Dec8) = True
type instance IsEven' (xh :. Dec9) = False

-- type family Div2 x
type instance Div2 (Dec x) = Dec (Div2' x)

type family Div2Digit x
type instance Div2Digit Dec0 = Dec0
type instance Div2Digit Dec1 = Dec0
type instance Div2Digit Dec2 = Dec1
type instance Div2Digit Dec3 = Dec1
type instance Div2Digit Dec4 = Dec2
type instance Div2Digit Dec5 = Dec2
type instance Div2Digit Dec6 = Dec3
type instance Div2Digit Dec7 = Dec3
type instance Div2Digit Dec8 = Dec4
type instance Div2Digit Dec9 = Dec4

type family Div2' x
type instance Div2' DecN       = DecN
type instance Div2' (Neg' x)   = Neg' (Div2Pos x)
type instance Div2' (xh :. xl) = Div2Pos (xh :. xl)

type family Div2Pos x
type instance Div2Pos (xh :. xl) = Normalize' (Div2Pos' xh (Div2Digit xl) (If (IsEven' xh) Dec0 Dec5))

type family Div2Pos' xh xl' rem
type instance Div2Pos' xh xl' rem =
    (AddCarry xl' rem (Div2' xh)) :. (AddDigit xl' rem)

---------------
-- Exponentiation

type instance Pow2 (Dec x) = Dec (Pow2' x (DecN :. Dec1))

type family Pow2' x y
type instance Pow2' (Neg' x)   y = DecN
type instance Pow2' DecN       y = y
type instance Pow2' (xh :. xl) y = Pow2' (Pred' (xh :. xl)) (Mul2' y)

---------------
-- Logarithm
type instance Log2Ceil (Dec x) = Dec (Log2C' (Pred' x) DecN)

type family Log2C' x y
type instance Log2C' (Neg' x) y   = DecN
type instance Log2C' DecN     y   = y
type instance Log2C' (xh :. xl) y = Log2C' (Div2' (xh :. xl)) (Succ' y)

---------------
-- Compare

type family CompareDigit x y
-- putStr $ unlines $ concat $ [ [ "type instance CompareDigit Dec" ++ show x ++ " Dec" ++ show y ++ " = " ++ (if x < y then "LT" else (if x > y then "GT" else "EQ")) | y <- [0..9] ] ++ [ "" ] | x <- [0..9] ]
type instance CompareDigit Dec0 Dec0 = EQ
type instance CompareDigit Dec0 Dec1 = LT
type instance CompareDigit Dec0 Dec2 = LT
type instance CompareDigit Dec0 Dec3 = LT
type instance CompareDigit Dec0 Dec4 = LT
type instance CompareDigit Dec0 Dec5 = LT
type instance CompareDigit Dec0 Dec6 = LT
type instance CompareDigit Dec0 Dec7 = LT
type instance CompareDigit Dec0 Dec8 = LT
type instance CompareDigit Dec0 Dec9 = LT

type instance CompareDigit Dec1 Dec0 = GT
type instance CompareDigit Dec1 Dec1 = EQ
type instance CompareDigit Dec1 Dec2 = LT
type instance CompareDigit Dec1 Dec3 = LT
type instance CompareDigit Dec1 Dec4 = LT
type instance CompareDigit Dec1 Dec5 = LT
type instance CompareDigit Dec1 Dec6 = LT
type instance CompareDigit Dec1 Dec7 = LT
type instance CompareDigit Dec1 Dec8 = LT
type instance CompareDigit Dec1 Dec9 = LT

type instance CompareDigit Dec2 Dec0 = GT
type instance CompareDigit Dec2 Dec1 = GT
type instance CompareDigit Dec2 Dec2 = EQ
type instance CompareDigit Dec2 Dec3 = LT
type instance CompareDigit Dec2 Dec4 = LT
type instance CompareDigit Dec2 Dec5 = LT
type instance CompareDigit Dec2 Dec6 = LT
type instance CompareDigit Dec2 Dec7 = LT
type instance CompareDigit Dec2 Dec8 = LT
type instance CompareDigit Dec2 Dec9 = LT

type instance CompareDigit Dec3 Dec0 = GT
type instance CompareDigit Dec3 Dec1 = GT
type instance CompareDigit Dec3 Dec2 = GT
type instance CompareDigit Dec3 Dec3 = EQ
type instance CompareDigit Dec3 Dec4 = LT
type instance CompareDigit Dec3 Dec5 = LT
type instance CompareDigit Dec3 Dec6 = LT
type instance CompareDigit Dec3 Dec7 = LT
type instance CompareDigit Dec3 Dec8 = LT
type instance CompareDigit Dec3 Dec9 = LT

type instance CompareDigit Dec4 Dec0 = GT
type instance CompareDigit Dec4 Dec1 = GT
type instance CompareDigit Dec4 Dec2 = GT
type instance CompareDigit Dec4 Dec3 = GT
type instance CompareDigit Dec4 Dec4 = EQ
type instance CompareDigit Dec4 Dec5 = LT
type instance CompareDigit Dec4 Dec6 = LT
type instance CompareDigit Dec4 Dec7 = LT
type instance CompareDigit Dec4 Dec8 = LT
type instance CompareDigit Dec4 Dec9 = LT

type instance CompareDigit Dec5 Dec0 = GT
type instance CompareDigit Dec5 Dec1 = GT
type instance CompareDigit Dec5 Dec2 = GT
type instance CompareDigit Dec5 Dec3 = GT
type instance CompareDigit Dec5 Dec4 = GT
type instance CompareDigit Dec5 Dec5 = EQ
type instance CompareDigit Dec5 Dec6 = LT
type instance CompareDigit Dec5 Dec7 = LT
type instance CompareDigit Dec5 Dec8 = LT
type instance CompareDigit Dec5 Dec9 = LT

type instance CompareDigit Dec6 Dec0 = GT
type instance CompareDigit Dec6 Dec1 = GT
type instance CompareDigit Dec6 Dec2 = GT
type instance CompareDigit Dec6 Dec3 = GT
type instance CompareDigit Dec6 Dec4 = GT
type instance CompareDigit Dec6 Dec5 = GT
type instance CompareDigit Dec6 Dec6 = EQ
type instance CompareDigit Dec6 Dec7 = LT
type instance CompareDigit Dec6 Dec8 = LT
type instance CompareDigit Dec6 Dec9 = LT

type instance CompareDigit Dec7 Dec0 = GT
type instance CompareDigit Dec7 Dec1 = GT
type instance CompareDigit Dec7 Dec2 = GT
type instance CompareDigit Dec7 Dec3 = GT
type instance CompareDigit Dec7 Dec4 = GT
type instance CompareDigit Dec7 Dec5 = GT
type instance CompareDigit Dec7 Dec6 = GT
type instance CompareDigit Dec7 Dec7 = EQ
type instance CompareDigit Dec7 Dec8 = LT
type instance CompareDigit Dec7 Dec9 = LT

type instance CompareDigit Dec8 Dec0 = GT
type instance CompareDigit Dec8 Dec1 = GT
type instance CompareDigit Dec8 Dec2 = GT
type instance CompareDigit Dec8 Dec3 = GT
type instance CompareDigit Dec8 Dec4 = GT
type instance CompareDigit Dec8 Dec5 = GT
type instance CompareDigit Dec8 Dec6 = GT
type instance CompareDigit Dec8 Dec7 = GT
type instance CompareDigit Dec8 Dec8 = EQ
type instance CompareDigit Dec8 Dec9 = LT

type instance CompareDigit Dec9 Dec0 = GT
type instance CompareDigit Dec9 Dec1 = GT
type instance CompareDigit Dec9 Dec2 = GT
type instance CompareDigit Dec9 Dec3 = GT
type instance CompareDigit Dec9 Dec4 = GT
type instance CompareDigit Dec9 Dec5 = GT
type instance CompareDigit Dec9 Dec6 = GT
type instance CompareDigit Dec9 Dec7 = GT
type instance CompareDigit Dec9 Dec8 = GT
type instance CompareDigit Dec9 Dec9 = EQ

type instance Compare (Dec x) (Dec y) = Compare' x y
type family Compare' x y
type instance Compare' (Neg' x)   (Neg' y)   = CompareNeg (ComparePos x y EQ)
type instance Compare' (Neg' x)   DecN       = LT
type instance Compare' (Neg' x)   (yh :. yl) = LT
type instance Compare' DecN       (Neg' y)   = GT
type instance Compare' DecN       DecN       = EQ
type instance Compare' DecN       (yh :. yl) = LT
type instance Compare' (xh :. xl) (Neg' y)   = GT
type instance Compare' (xh :. xl) DecN       = GT
type instance Compare' (xh :. xl) (yh :. yl) = ComparePos (xh :. xl) (yh :. yl) EQ

type family ComparePos x y c
type instance ComparePos DecN       DecN       c = c
type instance ComparePos DecN       (yh :. yl) c = LT
type instance ComparePos (xh :. xl) DecN       c = GT
type instance ComparePos (xh :. xl) (yh :. yl) GT = ComparePos' xh yh (CompareDigit xl yl) GT
type instance ComparePos (xh :. xl) (yh :. yl) EQ = ComparePos xh yh (CompareDigit xl yl)
type instance ComparePos (xh :. xl) (yh :. yl) LT = ComparePos' xh yh (CompareDigit xl yl) LT

type family ComparePos' x y c l
type instance ComparePos' x y LT c = ComparePos x y LT
type instance ComparePos' x y EQ c = ComparePos x y c
type instance ComparePos' x y GT c = ComparePos x y GT

type family CompareNeg c
type instance CompareNeg LT = GT
type instance CompareNeg EQ = EQ
type instance CompareNeg GT = LT
