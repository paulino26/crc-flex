{-|
Module      : CrcFlex
Description : Computes arbitrary CRCs over a stream of [Word8]
Copyright   : (c) Paul Randall, 2020
License     : MIT
Maintainer  : paul-randall2@sky.com
Stability   : experimental
Portability : POSIX

This module is based on the information and subsequent C code implementation
given "A PAINLESS GUIDE TO CRC ERROR DETECTION ALGORITHM", Ross N. Williams, 19th August 1993.
http://read.pudn.com/downloads137/doc/585979/crc-Ross.pdf

It is designed to allow the calculation of CRCs for arbitrary CRC widths where the underlying 
polynomial and options for initial value, final XOR and reflecting of the input / output can be
independently controlled.

It is not designed to be efficient in terms of speed or memory usag more to provide enough flexibility to 
cater to the lesser used CRCs that are not implemented in other packages.

There is a good catlog of CRCs and their implementation details here:-
https://reveng.sourceforge.io/crc-catalogue/
-}

module Data.Digest.CrcFlex (CrcModel(..), CrcTable(..), applyCrcModel, mkCrcTable, applyCrcTable
        , crc7Mmc, crc11FlexRay, crc8Maxim, crc16IBM, crcCCITT, crc32Iso, crc64Iso, crc64Emca
        ) where

import Data.Digest.CrcFlex.Internal
import Data.Bits (xor)
import Data.List (foldl')
import Data.Word (Word8)

-- |The 'crc7Mmc' function provides the model for the crc-7 (used for MMC devices).
crc7Mmc :: CrcModel
crc7Mmc = CrcModel 7 0x09 0 False 0

-- |The 'crc8Maxim' function provides the model for the Maxim crc-8 (used for 1-wire devices).
crc8Maxim :: CrcModel
crc8Maxim = CrcModel 8 0x31 0 True 0

-- |The 'crc11FlexRay' function provides the model for the FlexRay crc-11.
crc11FlexRay :: CrcModel
crc11FlexRay = CrcModel 11 0x385 0x01a False 0 

-- |The 'crc16IBM' function provides the model for the commonly used IBM crc-16.
crc16IBM :: CrcModel
crc16IBM = CrcModel 16 0x8005 0 True 0

-- |The 'crcCCITT' function provides the model for the commonly used CRC-16-CCITT.
crcCCITT :: CrcModel
crcCCITT = CrcModel 16 0x1021 0xFFFF False 0

-- |The 'crc32Iso' function provides the model for the commonly used CRC-32 (ISO).
crc32Iso :: CrcModel
crc32Iso = CrcModel 32 0x04C11DB7 0xFFFFFFFF True 0xFFFFFFFF

-- |The 'crc64Iso' function provides the model for the commonly used CRC-64 (ISO).
crc64Iso :: CrcModel
crc64Iso = CrcModel 64 0x1B 0xffffffffffffffff True 0xffffffffffffffff

-- |The 'crc64Emca' function provides the model for the commonly used CRC-64 (ISO).
crc64Emca :: CrcModel
crc64Emca = CrcModel 64 0x42f0e1eba9ea3693 0 False 0

-- |The 'applyCrcModel' function produces the resulting CRC from running the CrcModel over the input [Word8] stream.
applyCrcModel :: CrcModel -> [Word8] -> Integer
applyCrcModel m xs = c' `xor` cmXor m
    where
        c = foldl' (nextdigest m) (cmInit m) $ map fromIntegral xs
        c' = if cmReflect m then reflect (cmWidth m) c else c

-- |The 'applyCrcTable' function produces the resulting CRC by applying the CrcTable over the input [Word8] stream.
applyCrcTable :: CrcTable -> [Word8] -> Integer
applyCrcTable (CrcTable m nxt) xs = c `xor` cmXor m
    where
        st = if cmReflect m then reflect (cmWidth m) (cmInit m) else cmInit m
        c = foldl' nxt st $ map fromIntegral xs