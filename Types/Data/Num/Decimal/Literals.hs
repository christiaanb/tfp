module Types.Data.Num.Decimal.Literals where

import Types.Data.Num.Decimal.Literals.TH

import Types.Data.Num.Decimal.Digits
import Types.Data.Num.Ops

$( decLiteralsD "D" "d" (-10000) (10000) )
