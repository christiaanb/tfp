module Data.SizedInt
    ( SizedInt ) where

import Data.Bits
import Types

newtype (NaturalT nT) => SizedInt nT = SizedInt Integer

sizeT :: SizedInt nT
      -> nT
sizeT _ = undefined

mask :: forall nT . NaturalT nT
     => nT
     -> Integer
mask _ = bit (fromIntegerT (undefined :: nT)) - 1

signBit :: forall nT . NaturalT nT
        => nT
        -> Int
signBit _ = fromIntegerT (undefined :: nT) - 1

isNegative :: forall nT . NaturalT nT
           => SizedInt nT
           -> Bool
isNegative (SizedInt x) =
    testBit x $ signBit (undefined :: nT)

instance NaturalT nT => Eq (SizedInt nT) where
    (SizedInt x) == (SizedInt y) = x == y
    (SizedInt x) /= (SizedInt y) = x /= y

instance NaturalT nT => Show (SizedInt nT) where
    showsPrec prec n =
        showsPrec prec $ toInteger n

instance NaturalT nT => Read (SizedInt nT) where
    readsPrec prec str =
        [ (fromInteger n, str)
        | (n, str) <- readsPrec prec str ]

instance NaturalT nT => Ord (SizedInt nT) where
    a `compare` b = toInteger a `compare` toInteger b

instance NaturalT nT => Bounded (SizedInt nT) where
    minBound = SizedInt $ negate $ 1 `shiftL` (fromIntegerT (undefined :: nT) - 1)
    maxBound = SizedInt $ (1 `shiftL` (fromIntegerT (undefined :: nT) - 1)) - 1

instance NaturalT nT => Enum (SizedInt nT) where
    succ x
       | x == maxBound  = error $ "Enum.succ{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error $ "Enum.succ{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `pred' of minBound"
       | otherwise      = x - 1
    
    fromEnum (SizedInt x)
        | x > toInteger (maxBound :: Int) =
            error $ "Enum.fromEnum{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedInt greater than maxBound :: Int"
        | x < toInteger (minBound :: Int) =
            error $ "Enum.fromEnum{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedInt smaller than minBound :: Int"
        | otherwise =
            fromInteger x
    toEnum x
        | x' > toInteger (maxBound :: SizedInt nT) =
            error $ "Enum.fromEnum{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedInt greater than maxBound :: SizedInt " ++ show (fromIntegerT (undefined :: nT))
        | x' < toInteger (minBound :: SizedInt nT) =
            error $ "Enum.fromEnum{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to take `fromEnum' on SizedInt smaller than minBound :: SizedInt " ++ show (fromIntegerT (undefined :: nT))
        | otherwise =
            fromInteger x'
            where x' = toInteger x

instance NaturalT nT => Num (SizedInt nT) where
    (SizedInt a) + (SizedInt b) =
        fromInteger $ a + b
    (SizedInt a) * (SizedInt b) =
        fromInteger $ a * b
    negate (SizedInt n) =
        fromInteger $ (n `xor` mask (undefined :: nT)) + 1
    a - b =
        a + (negate b)
    
    fromInteger n
      | n > 0 =
        SizedInt $ n .&. mask (undefined :: nT)
    fromInteger n
      | n < 0 =
        negate $ fromInteger $ negate n
    fromInteger _ =
        SizedInt 0
    
    abs s
      | isNegative s =
          negate s
      | otherwise =
          s
    signum s
      | isNegative s =
          -1
      | s == 0 =
          0
      | otherwise =
          1

instance NaturalT nT => Real (SizedInt nT) where
    toRational n = toRational $ toInteger n

instance NaturalT nT => Integral (SizedInt nT) where
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
    toInteger s@(SizedInt x) =
        if isNegative s
           then let SizedInt x' = negate s in negate x'
           else x

instance NaturalT nT => Bits (SizedInt nT) where
    (SizedInt a) .&. (SizedInt b) = SizedInt $ a .&. b
    (SizedInt a) .|. (SizedInt b) = SizedInt $ a .|. b
    (SizedInt a) `xor` SizedInt b = SizedInt $ a `xor` b
    complement (SizedInt x) = SizedInt $ x `xor` mask (undefined :: nT)
    (SizedInt x) `shiftL` b
      | b < 0 = error $ "Bits.shiftL{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedInt $ mask (undefined :: nT) .&. (x `shiftL` b)
    s@(SizedInt x) `shiftR` b
      | b < 0 = error $ "Bits.shiftR{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to shift by negative amount"
      | isNegative s =
        SizedInt $ mask (undefined :: nT) .&.
            ((x `shiftR` b) .|. (mask (undefined :: nT) `shiftL` (fromIntegerT (undefined :: nT) - b)))
      | otherwise =
        SizedInt $ (mask (undefined :: nT)) .&. (x `shiftR` b)
    (SizedInt a) `rotateL` b
      | b < 0 =
        error $ "Bits.rotateL{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedInt $ mask (undefined :: nT) .&.
            ((a `shiftL` b) .|. (a `shiftR` (fromIntegerT (undefined :: nT) - b)))
    (SizedInt a) `rotateR` b
      | b < 0 =
        error $ "Bits.rotateR{SizedInt " ++ show (fromIntegerT (undefined :: nT)) ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedInt $ mask (undefined :: nT) .&.
            ((a `shiftR` b) .|. (a `shiftL` (fromIntegerT (undefined :: nT) - b)))
    bitSize _ = fromIntegerT (undefined :: nT)
    isSigned _ = True
