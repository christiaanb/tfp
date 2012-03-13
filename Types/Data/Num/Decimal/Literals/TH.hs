module Types.Data.Num.Decimal.Literals.TH where

import Language.Haskell.TH

import qualified Types.Data.Num.Decimal.Digits as D
import qualified Types.Data.Num.Ops as O

decLiteralT :: Integer -> Q Type
decLiteralT n = appT (conT (''D.Dec)) (decLiteralT' n)
    where decLiteralT' n | n < 0     = appT (conT ''D.Neg') (decLiteralT' (-n))
                         | n == 0    = conT ''D.DecN
                         | otherwise = appT (appT (conT ''(O.:.)) (decLiteralT' (n `div` 10))) (conT (case n `mod` 10 of
                                                  0 -> ''D.Dec0 
                                                  1 -> ''D.Dec1
                                                  2 -> ''D.Dec2
                                                  3 -> ''D.Dec3
                                                  4 -> ''D.Dec4
                                                  5 -> ''D.Dec5
                                                  6 -> ''D.Dec6
                                                  7 -> ''D.Dec7
                                                  8 -> ''D.Dec8
                                                  9 -> ''D.Dec9))

decLiteralV :: Integer -> Q Exp
decLiteralV n = sigE [| undefined |] (decLiteralT n)

decLiteralD :: String
            -> String
            -> Integer
            -> Q [Dec]
decLiteralD typePrefix valPrefix n =
    do let suffix = if n < 0 then "N" ++ show (-n) else show n
           typeName = mkName $ typePrefix ++ suffix
           valName = mkName $ valPrefix ++ suffix
       tySyn <- tySynD typeName [] (decLiteralT n)
       sig   <- sigD valName (conT typeName)
       val   <- valD (varP valName) (normalB [| undefined |]) []
       return [ tySyn, sig, val ]

decLiteralsD :: String
             -> String
             -> Integer
             -> Integer
             -> Q [Dec]
decLiteralsD typePrefix valPrefix from to =
    fmap concat $ sequence $ [ decLiteralD typePrefix valPrefix n | n <- [from..to] ]
