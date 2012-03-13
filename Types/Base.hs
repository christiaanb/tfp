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

module Types.Base
    where

import qualified Prelude

type family Id x
type instance Id x = x

_T :: a
_T = Prelude.undefined
