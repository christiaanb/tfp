module Main where

import qualified Test.QuickCheck as Q

import qualified Prelude
import Data.Char (intToDigit)
import Control.Monad (when)

import Types
import Types.Data.Num.Decimal.Literals.TH

testIsPositive1 :: IsPositive D1 -> True
testIsPositive1 = Prelude.id
testIsPositive2 :: IsPositive D0 -> False
testIsPositive2 = Prelude.id
testIsPositive3 :: IsPositive DN1 -> False
testIsPositive3 = Prelude.id
testIsPositive4 :: IsPositive D10 -> True
testIsPositive4 = Prelude.id
testIsPositive5 :: IsPositive DN10 -> False
testIsPositive5 = Prelude.id

testIsNegative1 :: IsNegative D1 -> False
testIsNegative1 = Prelude.id
testIsNegative2 :: IsNegative D0 -> False
testIsNegative2 = Prelude.id
testIsNegative3 :: IsNegative DN1 -> True
testIsNegative3 = Prelude.id
testIsNegative4 :: IsNegative D10 -> False
testIsNegative4 = Prelude.id
testIsNegative5 :: IsNegative DN10 -> True
testIsNegative5 = Prelude.id

testIsZero1 :: IsZero D1 -> False
testIsZero1 = Prelude.id
testIsZero2 :: IsZero D0 -> True
testIsZero2 = Prelude.id
testIsZero3 :: IsZero DN1 -> False
testIsZero3 = Prelude.id
testIsZero4 :: IsZero D10 -> False
testIsZero4 = Prelude.id
testIsZero5 :: IsZero DN10 -> False
testIsZero5 = Prelude.id

testSucc1 :: Succ D0 -> D1
testSucc1 = Prelude.id
testSucc2 :: Succ D9 -> D10
testSucc2 = Prelude.id
testSucc3 :: Succ DN1 -> D0
testSucc3 = Prelude.id
testSucc4 :: Succ D99 -> D100
testSucc4 = Prelude.id
testSucc5 :: Succ DN100 -> DN99
testSucc5 = Prelude.id
testSucc6 :: Succ D100 -> D101
testSucc6 = Prelude.id
testSucc7 :: Succ DN101 -> DN100
testSucc7 = Prelude.id
testSucc8 :: Succ D0 -> D1 :+: D0
testSucc8 = Prelude.id
testSucc9 :: Succ D0 -> D0 :+: D1
testSucc9 = Prelude.id
testSucc10 :: Succ D9 -> D1 :+: D9
testSucc10 = Prelude.id
testSucc11 :: Succ D9 -> D9 :+: D1
testSucc11 = Prelude.id

testPred1 :: Pred D1 -> D0
testPred1 = Prelude.id
testPred2 :: Pred D0 -> DN1
testPred2 = Prelude.id
testPred3 :: Pred DN1 -> DN2
testPred3 = Prelude.id
testPred4 :: Pred DN9 -> DN10
testPred4 = Prelude.id
testPred5 :: Pred D10 -> D9
testPred5 = Prelude.id
testPred6 :: Pred DN99 -> DN100
testPred6 = Prelude.id
testPred7 :: Pred D100 -> D99
testPred7 = Prelude.id
testPred8 :: Pred D0 -> D0 :-: D1
testPred8 = Prelude.id
testPred9 :: Pred D10 -> D10 :-: D1
testPred9 = Prelude.id
testPred10 :: Pred D9 -> D8
testPred10 = Prelude.id
testPred11 :: Pred D8 -> D7
testPred11 = Prelude.id
testPred12 :: Pred D7 -> D6
testPred12 = Prelude.id
testPred13 :: Pred D6 -> D5
testPred13 = Prelude.id
testPred14 :: Pred D5 -> D4
testPred14 = Prelude.id
testPred15 :: Pred D4 -> D3
testPred15 = Prelude.id
testPred16 :: Pred D3 -> D2
testPred16 = Prelude.id
testPred17 :: Pred D2 -> D1
testPred17 = Prelude.id
testPred18 :: Pred D1 -> D0
testPred18 = Prelude.id

testAdd1 :: D0 :+: D0 -> D0
testAdd1 = Prelude.id
testAdd2 :: DN1 :+: D1 -> D0
testAdd2 = Prelude.id
testAdd3 :: D1 :+: DN1 -> D0
testAdd3 = Prelude.id
testAdd4 :: D1 :+: D1 -> D2
testAdd4 = Prelude.id
testAdd5 :: D9 :+: D1 -> D10
testAdd5 = Prelude.id
testAdd6 :: D10 :+: DN1 -> D9
testAdd6 = Prelude.id
testAdd7 :: D100 :+: DN1 -> D99
testAdd7 = Prelude.id
testAdd8 :: D100 :+: DN10 -> D90
testAdd8 = Prelude.id

testSub1 :: D0 :-: D0 -> D0
testSub1 = Prelude.id
testSub2 :: D1 :-: D0 -> D1
testSub2 = Prelude.id
testSub3 :: D0 :-: D1 -> DN1
testSub3 = Prelude.id
testSub4 :: DN1 :-: D0 -> DN1
testSub4 = Prelude.id
testSub5 :: D0 :-: DN1 -> D1
testSub5 = Prelude.id
testSub6 :: D100 :-: D1 -> D99
testSub6 = Prelude.id
testSub7 :: DN100 :-: D1 -> DN101
testSub7 = Prelude.id
testSub8 :: D100 :-: DN1 -> D101
testSub8 = Prelude.id
testSub9 :: DN100 :-: DN1 -> DN99
testSub9 = Prelude.id
testSub10 :: D1 :-: D100 -> DN99
testSub10 = Prelude.id
testSub11 :: DN1 :-: D100 -> DN101
testSub11 = Prelude.id
testSub12 :: D1 :-: DN100 -> D101
testSub12 = Prelude.id
testSub13 :: DN1 :-: DN100 -> D99
testSub13 = Prelude.id
testSub14 :: D57 :-: D58 -> DN1
testSub14 = Prelude.id
testSub15 :: D1000 :-: D11 -> D989
testSub15 = Prelude.id

testMul1 :: D0 :*: D0 -> D0
testMul1 = Prelude.id
testMul2 :: D1 :*: D1 -> D1
testMul2 = Prelude.id
testMul3 :: D0 :*: D1 -> D0
testMul3 = Prelude.id
testMul4 :: D1 :*: D0 -> D0
testMul4 = Prelude.id
testMul5 :: D1 :*: DN1 -> DN1
testMul5 = Prelude.id
testMul6 :: DN1 :*: D1 -> DN1
testMul6 = Prelude.id
testMul7 :: DN1 :*: DN1 -> D1
testMul7 = Prelude.id
testMul8 :: D100 :*: D100 -> D10000
testMul8 = Prelude.id
testMul9 :: D17 :*: D31 -> D527
testMul9 = Prelude.id

testFac1 :: Fac D0 -> D1
testFac1 = Prelude.id
testFac2 :: Fac D1 -> D1
testFac2 = Prelude.id
testFac3 :: Fac D6 -> D720
testFac3 = Prelude.id
$( decLiteralD "D" "d" 3628800 )
testFac4 :: Fac D10 -> D3628800
testFac4 = Prelude.id

testEQ1 :: D0 :==: D0 -> True
testEQ1 = Prelude.id
testEQ2 :: D0 :==: Pred D1 -> True
testEQ2 = Prelude.id
testEQ3 :: (D1 :+: D9) :==: D10 -> True
testEQ3 = Prelude.id
testEQ4 :: (D1 :+: D9) :==: D11 -> False
testEQ4 = Prelude.id
testEQ5 :: D9 :==: D0 -> False
testEQ5 = Prelude.id
testEQ6 :: D8 :==: D0 -> False
testEQ6 = Prelude.id
testEQ7 :: D7 :==: D0 -> False
testEQ7 = Prelude.id
testEQ8 :: D6 :==: D0 -> False
testEQ8 = Prelude.id
testEQ9 :: D5 :==: D0 -> False
testEQ9 = Prelude.id
testEQ10 :: D4 :==: D0 -> False
testEQ10 = Prelude.id
testEQ11 :: D3 :==: D0 -> False
testEQ11 = Prelude.id
testEQ12 :: D2 :==: D0 -> False
testEQ12 = Prelude.id
testEQ13 :: D1 :==: D0 -> False
testEQ13 = Prelude.id

testMin1 :: Min D0 D5 -> D0
testMin1 = Prelude.id
testMin2 :: Min D5 D0 -> D0
testMin2 = Prelude.id
testMin3 :: Min DN5 D5 -> DN5
testMin3 = Prelude.id

testMax1 :: Max D1 D6 -> D6
testMax1 = Prelude.id
testMax2 :: Max DN6 D6 -> D6
testMax2 = Prelude.id
testMax3 :: Max D6 D1 -> D6
testMax3 = Prelude.id

testLog1 :: Log2Ceil D8 -> D3
testLog1 = Prelude.id
testLog2 :: Log2Ceil D9 -> D4
testLog2 = Prelude.id
testLog3 :: Log2Ceil D2 -> D1
testLog3 = Prelude.id
testLog4 :: Log2Ceil D1 -> D0
testLog4 = Prelude.id

class TestIter n zero where
    testIter :: n -> zero -> Prelude.String

instance ( NaturalT n
         , (n :==: D0) ~ True )
  => TestIter n True where
    testIter _ _ = ""

instance ( NaturalT n
         , (n :==: D0) ~ False
         , TestIter (Pred n) ((Pred n) :==: D0) )
  => TestIter n False where
    testIter n _ =
        intToDigit (fromIntegerT n) : testIter (_T :: (Pred n)) (_T :: ((Pred n) :==: D0))

main = do
  let testIterResult = testIter d9 (_T :: False)
  when (testIterResult Prelude./= "987654321") (Prelude.putStrLn ("testIter failed, got: " Prelude.++ testIterResult))
  Q.quickCheck prop_reifyIntegral

prop_reifyIntegral i = reifyIntegral (Prelude.undefined :: Decimal) i fromIntegerT Prelude.== i
