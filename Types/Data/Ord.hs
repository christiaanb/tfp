-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Decimal.Digits
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

module Types.Data.Ord
    ( Compare
    , compareT
    , LT
    , EQ
    , GT
    , IsLT
    , isLTT
    , IsEQ
    , isEQT
    , IsGT
    , isGTT
    , (:<:)
    , ltT
    , (:<=:)
    , leT
    , (:==:)
    , eqT
    , (:>=:)
    , geT
    , (:>:)
    , gtT
    , Min
    , minT
    , Max
    , maxT
    ) where

import qualified Prelude

import Types.Data.Bool

type family Compare x y
data LT
data EQ
data GT
compareT :: x -> y -> Compare x y
compareT _ _ = Prelude.undefined

type family IsLT c
type instance IsLT LT = True
type instance IsLT EQ = False
type instance IsLT GT = False
isLTT :: c -> IsLT c
isLTT _ = Prelude.undefined

type family IsEQ c
type instance IsEQ LT = False
type instance IsEQ EQ = True
type instance IsEQ GT = False
isEQT :: c -> IsEQ c
isEQT _ = Prelude.undefined

type family IsGT c
type instance IsGT LT = False
type instance IsGT EQ = False
type instance IsGT GT = True
isGTT :: c -> IsGT c
isGTT _ = Prelude.undefined

type instance Compare LT LT = EQ
type instance Compare LT EQ = LT
type instance Compare LT GT = LT
type instance Compare EQ LT = GT
type instance Compare EQ EQ = EQ
type instance Compare EQ GT = LT
type instance Compare GT LT = GT
type instance Compare GT EQ = GT
type instance Compare GT GT = EQ

type family x :<: y
type instance x :<: y = IsLT (Compare x y)
ltT :: x -> y -> x :<: y
ltT _ _ = Prelude.undefined
class LTT x y
instance ((x :<: y) ~ True) => LTT x y

type family x :<=: y
type instance x :<=: y = Not (x :>: y)
leT :: x -> y -> x :<=: y
leT _ _ = Prelude.undefined
class LET x y
instance ((x :<=: y) ~ True) => LET x y

type family x :==: y
type instance x :==: y = IsEQ (Compare x y)
eqT :: x -> y -> x :==: y
eqT _ _ = Prelude.undefined
class EQT x y
instance ((x :==: y) ~ True) => EQT x y

type family x :/=: y
type instance x :/=: y = Not (x :==: y)
neT :: x -> y -> x :/=: y
neT _ _ = Prelude.undefined
class NET x y
instance ((x :/=: y) ~ True) => NET x y

type family x :>=: y
type instance x :>=: y = Not (x :<: y)
geT :: x -> y -> x :>=: y
geT _ _ = Prelude.undefined
class GET x y
instance ((x :>=: y) ~ True) => GET x y

type family x :>: y
type instance x :>: y = IsGT (Compare x y)
gtT :: x -> y -> x :>: y
gtT _ _ = Prelude.undefined
class GTT x y
instance ((x :>: y) ~ True) => GTT x y

type family Min x y
type instance Min x y = If (x :<=: y) x y
minT :: x -> y -> Min x y
minT _ _ = Prelude.undefined

type family Max x y
type instance Max x y = If (x :>=: y) x y
maxT :: x -> y -> Max x y
maxT _ _ = Prelude.undefined

type instance Compare False False = EQ
type instance Compare False True  = LT
type instance Compare True  False = GT
type instance Compare True  True  = EQ
