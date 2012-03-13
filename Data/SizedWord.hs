module Data.SizedWord
    ( SizedWord ) where

import Data.Bits
import Types

newtype (NaturalT nT) => SizedWord nT = SizedWord Integer

sizeT :: SizedWord nT
      -> nT
sizeT _ = undefined

mask :: forall nT . NaturalT nT
     => nT
     -> Integer
mask _ = bit (fromIntegerT (undefined :: nT)) - 1

instance NaturalT nT => Eq (SizedWord nT) where
    (SizedWord x) == (SizedWord y) = x == y
    (SizedWord x) /= (SizedWord y) = x /= y

instance NaturalT nT => Show (SizedWord nT) where
    showsPrec prec n =
        showsPrec prec $ toInteger n

instance NaturalT nT => Read (SizedWord nT) where
    readsPrec prec str =
        [ (fromInteger n, str)
        | (n, str) <- readsPrec prec str ]

instance NaturalT nT => Ord (SizedWord nT) where
    a `compare` b = toInteger a `compare` toInteger b

instance NaturalT nT => Bounded (SizedWord nT) where
    minBound = 0
    maxBound = SizedWord $ (1 `shiftL` (fromIntegerT (undefined :: nT))) - 1

instance NaturalT nT => Enum (SizedWord nT) where
    succ x
       | x == maxBound  = error $ "Enum.succ{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error $ "Enum.succ{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `pred' of minBound"
       | otherwise      = x - 1
    
    fromEnum (SizedWord x)
        | x > toInteger (maxBound :: Int) =
            error $ "Enum.fromEnum{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedWord greater than maxBound :: Int"
        | x < toInteger (minBound :: Int) =
            error $ "Enum.fromEnum{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedWord smaller than minBound :: Int"
        | otherwise =
            fromInteger x
    toEnum x
        | x > fromIntegral (maxBound :: SizedWord nT) =
            error $ "Enum.fromEnum{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedWord greater than maxBound :: SizedWord " ++ show (fromIntegerT (undefined :: nT))
        | x < fromIntegral (minBound :: SizedWord nT) =
            error $ "Enum.fromEnum{SizedWord " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedWord smaller than minBound :: SizedWord " ++ show (fromIntegerT (undefined :: nT))
        | otherwise =
            fromInteger $ toInteger x

instance NaturalT nT => Num (SizedWord nT) where
    (SizedWord a) + (SizedWord b) =
        fromInteger $ a + b
    (SizedWord a) * (SizedWord b) =
        fromInteger $ a * b
    negate s@(SizedWord n) =
        fromInteger $ (n `xor` mask (sizeT s)) + 1
    a - b =
        a + (negate b)
    
    fromInteger n
      | n > 0 =
        SizedWord $ n .&. mask (undefined :: nT)
    fromInteger n
      | n < 0 =
        negate $ fromInteger $ negate n
    fromInteger _ =
        SizedWord 0
    
    abs s = s
    signum s
      | s == 0 =
          0
      | otherwise =
          1

instance NaturalT nT => Real (SizedWord nT) where
    toRational n = toRational $ toInteger n

instance NaturalT nT => Integral (SizedWord nT) where
    a `quot` b =
        fromInteger $ toInteger a `quot` toInteger b
    a `rem` b =
        fromInteger $ toInteger a `rem` toInteger b
    a `div` b =
        fromInteger $ toInteger a `div` toInteger b
    a `mod` b =
        fromInteger $ toInteger a `mod` toInteger b
    a `quotRem` b =
        let (quot, rem) = toInteger a `quotRem` toInteger b
        in (fromInteger quot, fromInteger rem)
    a `divMod` b =
        let (div, mod) = toInteger a `divMod` toInteger b
        in (fromInteger div, fromInteger mod)
    toInteger s@(SizedWord x) = x

instance NaturalT nT => Bits (SizedWord nT) where
    (SizedWord a) .&. (SizedWord b) = SizedWord $ a .&. b
    (SizedWord a) .|. (SizedWord b) = SizedWord $ a .|. b
    (SizedWord a) `xor` SizedWord b = SizedWord $ a `xor` b
    complement (SizedWord x) = SizedWord $ x `xor` mask (undefined :: nT)
    s@(SizedWord x) `shiftL` b
      | b < 0 = error $ "Bits.shiftL{SizedWord " ++ show (bitSize s) ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedWord $ mask (undefined :: nT) .&. (x `shiftL` b)
    s@(SizedWord x) `shiftR` b
      | b < 0 = error $ "Bits.shiftR{SizedWord " ++ show (bitSize s) ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedWord $ (x `shiftR` b)
    s@(SizedWord x) `rotateL` b
      | b < 0 =
        error $ "Bits.rotateL{SizedWord " ++ show (bitSize s) ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedWord $ mask (undefined :: nT) .&.
            ((x `shiftL` b) .|. (x `shiftR` (bitSize s - b)))
    s@(SizedWord x) `rotateR` b
      | b < 0 =
        error $ "Bits.rotateR{SizedWord " ++ show (bitSize s) ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedWord $ mask (undefined :: nT) .&.
            ((x `shiftR` b) .|. (x `shiftL` (bitSize s - b)))
    bitSize _ = fromIntegerT (undefined :: nT)
    isSigned _ = False
