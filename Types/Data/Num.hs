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
    , reifyIntegralD
    , reifyPositiveD
    , reifyNegativeD
    ) where

import Types.Data.Num.Ops
import Types.Data.Num.Decimal

reifyIntegralD = reifyIntegral decimal
reifyPositiveD = reifyPositive decimal
reifyNegativeD = reifyNegative decimal