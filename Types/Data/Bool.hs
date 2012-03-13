-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Bool
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

module Types.Data.Bool
    ( True
    , trueT
    , False
    , falseT
    , Not
    , notT
    , (:&&:)
    , andT
    , (:||:)
    , orT
    , IfT(..)
    ) where

import Data.Typeable

import qualified Prelude

data True deriving (Typeable)
trueT :: True
trueT = Prelude.undefined
instance Prelude.Show True where
    show _ = "True"
data False deriving (Typeable)
falseT :: False
falseT = Prelude.undefined
instance Prelude.Show False where
    show _ = "False"

type family Not x
type instance Not False = True
type instance Not True  = False
notT :: x -> Not x
notT _ = Prelude.undefined

type family x :&&: y
type instance False :&&: x = False
type instance True  :&&: x = x
andT :: x -> y -> x :&&: y
andT _ _ = Prelude.undefined

type family x :||: y
type instance True  :||: x = True
type instance False :||: x = x
orT :: x -> y -> x :||: y
orT _ _ = Prelude.undefined

class IfT x y z where
    type If x y z
    ifT :: x -> y -> z -> If x y z
instance IfT True y z where
    type If True y z = y
    ifT _ y _ = y
instance IfT False y z where
    type If False y z = z
    ifT _ _ z = z
