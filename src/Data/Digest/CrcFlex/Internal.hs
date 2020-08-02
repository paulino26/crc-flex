module Data.Digest.CrcFlex.Internal where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), xor, shift, shiftR, testBit, complement)
import Data.List (foldl')

-- |The 'CrcModel' holds all of the information to model a CRC algorithm.
data CrcModel = CrcModel {
    cmWidth :: Int,      -- ^ specifies the width (in bits) of the CRC
    cmPoly :: Integer,   -- ^ specifies the polynomial to be used for the CRC calculation
    cmInit :: Integer,   -- ^ specifies the initial vlaue for the remainder for the CRC calculation
    cmReflect :: Bool,   -- ^ specifies whether the CRC uses the "reflected" form
    cmXor :: Integer     -- ^ specifies a value to XOR the final remainder (after reflection) with during the CRC calculation
}

-- |The 'CrcTable' holds the information required to generate a CRC using a table driven approach
data CrcTable = CrcTable {
    ctModel :: CrcModel,
    ctNext :: Integer -> Integer -> Integer
} 

-- |The 'pow2' function produces the 2^n for a given n.
-- This only works for n >= 0.
pow2 :: (Num b, Enum b, Eq b) => Int -> b
pow2 = (map pow2' [0..] !!)
    where
        pow2' :: (Num b, Enum b, Eq b) => Int -> b
        pow2' 0 = 1 
        pow2' n = 2 * pow2' (n-1)

-- |The 'reflect' function reverses the n least significant bits of x.
reflect :: Int -> Integer -> Integer
reflect n x = (x .&. mask) + refbits
    where
        ms = pow2 <$> reverse [0..(n-1)]
        ss = take n $ iterate (+2) (-(n-1))
        mask = complement $ pow2 n - 1
        refbits = sum $ zipWith (\m s -> shift (x .&. m) s) ms ss

-- |The 'toBits' function returns the bit pattern for the input (x)
-- It needs to be passed the input width (bits, w)
-- The resulting bit stream is represented as [Bool] and is ordered lsb to msb
toBits :: Int -> Integer -> [Bool]
toBits w x = [testBit x i | i <- [0..w-1]]

-- |The 'nextdigestbit' function uses CrcModel (m), the current remainder (x) and the next bit (b) to produces the next remainder based on the CRC polynomial.
nextdigestbit :: CrcModel -> Integer -> Bool -> Integer
nextdigestbit m x b = if (/=) (testBit x (cmWidth m - 1)) b then shift x 1 `xor` cmPoly m else shift x 1

-- |The 'nextdigest' function takes CrcModel m, the current CRC x and the next byte x
-- It iterates (using the CrcModel) for each of the 8 bits of the input byte and returns the resulting remainder.
nextdigest :: CrcModel -> Integer -> Integer -> Integer
nextdigest m crc x = crc' .&. wndmask
    where
        wndmask = pow2 (cmWidth m) - 1
        x' = if not (cmReflect m) then fromInteger (reflect 8 x) else fromInteger x
        crc' = foldl' (nextdigestbit m) crc $ toBits 8 x'

-- |Used to update the remainder (for normal form CrcModels - i.e. cmReflect == False) based on the next byte and a lookup table.
nextTableByte :: Array Int Integer -> Int -> Integer -> Integer -> Integer
nextTableByte ts w crc x = (shift crc 8 `xor` (ts ! index)) .&. wndmask
    where
        wndmask = pow2 w - 1
        highbyte = shiftR crc (w - 8)
        index = fromInteger (x `xor` highbyte)

-- |Used to update the remainder (for reversed form CrcModels - i.e. cmReflect == True) based on the next byte and a lookup table.
nextTableByteReflected :: Array Int Integer -> Int -> Integer -> Integer -> Integer
nextTableByteReflected ts w crc x = (shiftR crc 8 `xor` (ts ! index)) .&. wndmask
    where
        wndmask = pow2 w - 1
        lowbyte = crc .&. 0xFF       
        index = fromInteger (x `xor` lowbyte) 

-- |Generates a CRC table for a given CrcModel.
mkCrcTable :: CrcModel -> CrcTable
mkCrcTable m = CrcTable m nxt
    where
        genIndex = nextdigest m 0
        mayReflect = if cmReflect m then reflect (cmWidth m) else id
        ts = listArray (0, 255) $ map (mayReflect . genIndex) [0..255]
        nxt = if cmReflect m then nextTableByteReflected ts (cmWidth m) else nextTableByte ts (cmWidth m)