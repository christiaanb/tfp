{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Num
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

module Types.Data.Num
    ( module Types.Data.Num.Ops
    , module Types.Data.Num.Decimal
#if __GLASGOW_HASKELL__ >= 704 || __GLASGOW_HASKELL__ < 700
    , reifyIntegralD
    , reifyPositiveD
    , reifyNegativeD
#endif
    ) where

import Types.Data.Num.Ops
import Types.Data.Num.Decimal

-- An explanation of the following is in order:

-- Versions of GHC prior to 7.0 (e.g., 6.12.3) will compile this code
-- as long as the type signature isn't there.
-- Versions 7.0 and later, but before 7.4 will not compile it at all,
-- with or without the type signature.
-- Version 7.4 and later handles everything just fine.

#if __GLASGOW_HASKELL__ >= 704 || __GLASGOW_HASKELL__ < 700
#if __GLASGOW_HASKELL__ >= 704
reifyIntegralD :: Integer -> (forall s. (IntegerT s, Repr s ~ Decimal) => s -> a) -> a
#endif
reifyIntegralD = reifyIntegral decimal
#if __GLASGOW_HASKELL__ >= 704
reifyPositiveD :: Integer -> (forall s. (PositiveT s, Repr s ~ Decimal) => s -> a) -> Maybe a
#endif
reifyPositiveD = reifyPositive decimal
#if __GLASGOW_HASKELL__ >= 704
reifyNegativeD :: Integer -> (forall s. (NegativeT s, Repr s ~ Decimal) => s -> a) -> Maybe a
#endif
reifyNegativeD = reifyNegative decimal
#endif
