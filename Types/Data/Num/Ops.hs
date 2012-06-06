-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Decimal.Ops
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

module Types.Data.Num.Ops
    ( (:.)
    , Neg
    , negT
    , IsPositive
    , isPositiveT
    , IsZero
    , isZeroT
    , IsNegative
    , isNegativeT
    , IsNatural
    , isNaturalT
    , Succ
    , succT
    , Pred
    , predT
    , IsEven
    , isEvenT
    , IsOdd
    , isOddT
    , (:+:)
    , addT
    , (:-:)
    , subT
    , (:*:)
    , mulT
    , Mul2
    , mul2T
    , Pow2
    , pow2T
    , Log2
    , log2T
    , DivMod
    , divModT
    , Div
    , divT
    , Mod
    , modT
    , Div2
    , div2T
    , Fac
    , facT
    , IntegerR (..)
    , IntegerT (..)
    , NaturalT
    , PositiveT
    , NegativeT
    , reifyPositive
    , reifyNegative
    , reifyNatural
    ) where

import Types.Data.Bool

data ds :. d
instance (Show ds, Show d) => Show (ds :. d) where
    show _ = show (undefined :: ds) ++ show (undefined :: d)

-- | @Neg x@ evaluates to the additive inverse of (i.e., minus) @x@.
type family Neg x
negT :: x -> Neg x
negT _ = undefined

type family IsPositive x
isPositiveT :: x -> IsPositive x
isPositiveT _ = undefined

type family IsZero x
isZeroT :: x -> IsZero x
isZeroT _ = undefined

type family IsNegative x
isNegativeT :: x -> IsNegative x
isNegativeT _ = undefined

type family IsNatural x
isNaturalT :: x -> IsNatural x
isNaturalT _ = undefined

type family Succ x
succT :: x -> Succ x
succT _ = undefined

type family Pred x
predT :: x -> Pred x
predT _ = undefined

type family IsEven x
isEvenT :: x -> IsEven x
isEvenT _ = undefined

type family IsOdd x
type instance IsOdd x = Not (IsEven x)
isOddT :: x -> IsOdd x
isOddT _ = undefined

type family x :+: y
addT :: x -> y -> x :+: y
addT _ _ = undefined

type family x :-: y
subT :: x -> y -> x :-: y
subT _ _ = undefined

type family x :*: y
mulT :: x -> y -> x :*: y
mulT _ _ = undefined

type family Mul2 x
mul2T :: x -> Mul2 x
mul2T _ = undefined

type family DivMod x y
divModT :: x -> y -> DivMod x y
divModT _ _ = undefined

type family Div x y
divT :: x -> y -> Div x y
divT _ _ = undefined

type family Mod x y
modT :: x -> y -> Mod x y
modT _ _ = undefined

type family Div2 x
div2T :: x -> Div2 x
div2T _ = undefined

type family Fac x
facT :: x -> Fac x
facT _ = undefined

type family Pow2 x
pow2T :: x -> Pow2 x
pow2T _ = undefined

type family Log2 x
log2T :: x -> Log2 x
log2T _ = undefined

class IntegerT x => NaturalT x
instance (IntegerT x, IsNatural x  ~ True) => NaturalT x
class IntegerT x => PositiveT x
instance (IntegerT x, IsPositive x ~ True) => PositiveT x
class IntegerT x => NegativeT x
instance (IntegerT x, IsNegative x ~ True) => NegativeT x

class (IntegerR (Repr x)) => IntegerT x where
    fromIntegerT :: Num y => x -> y
    type Repr x

class IntegerR r where
    reifyIntegral :: r -> Integer -> (forall s. (IntegerT s, Repr s ~ r) => s -> a) -> a


--- positive and negative assertions: unsafe, in a trusted kernel
data AssertPos x
data AssertNeg x
data AssertNat x

assertPos :: x -> AssertPos x
assertPos _ = undefined

assertNeg :: x -> AssertNeg x
assertNeg _ = undefined

assertNat :: x -> AssertNat x
assertNat _ = undefined

type instance IsPositive (AssertPos x) = True
type instance IsPositive (AssertNeg x) = False

type instance IsNegative (AssertPos x) = False
type instance IsNegative (AssertNeg x) = True
type instance IsNegative (AssertNat x) = False

type instance IsNatural  (AssertPos x) = True
type instance IsNatural  (AssertNeg x) = False
type instance IsNatural  (AssertNat x) = True

instance IntegerT x => IntegerT (AssertPos x) where 
    fromIntegerT _ = fromIntegerT (undefined :: x)
    type Repr (AssertPos x) = Repr x

instance IntegerT x => IntegerT (AssertNeg x) where
    fromIntegerT _ = fromIntegerT (undefined :: x)
    type Repr (AssertNeg x) = Repr x

instance IntegerT x => IntegerT (AssertNat x) where
    fromIntegerT _ = fromIntegerT (undefined :: x)
    type Repr (AssertNat x) = Repr x

reifyPositive :: IntegerR r => r -> Integer -> (forall s. (PositiveT s, Repr s ~ r) => s -> a) -> Maybe a
reifyPositive r n k | n > 0     = Just (reifyIntegral r n (k . assertPos))
                    | otherwise = Nothing

reifyNegative :: IntegerR r => r -> Integer -> (forall s. (NegativeT s, Repr s ~ r) => s -> a) -> Maybe a
reifyNegative r n k | n < 0     = Just (reifyIntegral r n (k . assertNeg))
                    | otherwise = Nothing
reifyNatural :: IntegerR r => r -> Integer -> (forall s. (NaturalT s, Repr s ~ r) => s -> a) -> Maybe a
reifyNatural r n k | n >= 0     = Just (reifyIntegral r n (k . assertNat))
                    | otherwise = Nothing
