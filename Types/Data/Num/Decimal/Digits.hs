-----------------------------------------------------------------------------
-- |
-- Module      :  Types.Data.Num.Decimal.Digits
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

module Types.Data.Num.Decimal.Digits
    where

import Data.Typeable

import Types.Data.List
import Types.Data.Num.Ops

-- | Representation name for decimal type level numbers.
data Decimal
decimal = undefined :: Decimal

-- | The wrapper type for decimal type level numbers.
data Dec x
data Neg' x

-- | The terminator type for decimal digit lists.
data DecN
instance Show DecN where
    show _ = ""

data Dec0 deriving (Typeable)
instance Show Dec0 where
    show _ = "0"
data Dec1 deriving (Typeable)
instance Show Dec1 where
    show _ = "1"
data Dec2 deriving (Typeable)
instance Show Dec2 where
    show _ = "2"
data Dec3 deriving (Typeable)
instance Show Dec3 where
    show _ = "3"
data Dec4 deriving (Typeable)
instance Show Dec4 where
    show _ = "4"
data Dec5 deriving (Typeable)
instance Show Dec5 where
    show _ = "5"
data Dec6 deriving (Typeable)
instance Show Dec6 where
    show _ = "6"
data Dec7 deriving (Typeable)
instance Show Dec7 where
    show _ = "7"
data Dec8 deriving (Typeable)
instance Show Dec8 where
    show _ = "8"
data Dec9 deriving (Typeable)
instance Show Dec9 where
    show _ = "9"
