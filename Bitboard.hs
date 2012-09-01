module Bitboard (
  module Movement,
  Bitboard, flattenSq, unflattenSq, toBitboard, showBin, show2D) where

import Data.Bits ((.&.), (.|.), shiftL, bit)
import Data.List(foldl', splitAt)
import Data.Word (Word64)

import Movement

--Functions dealing with bitboards; i.e. representations of a chessboard as a 64-bit number.

type Bitboard = Word64

--little endian
flattenSq :: Position -> Int
flattenSq (x, y) = 8*y + x

unflattenSq :: Int -> Position
unflattenSq i = (i `mod` 8, i `div` 8)

toBitboard :: [Position] -> Bitboard
toBitboard = foldl' (.|.) 0 . map (bit . flattenSq)

showBin :: Bitboard -> String
showBin w = map (showBit . (w .&.) . (1 `shiftL`)) [0..63]
  where showBit 0 = '.'
        showBit _ = '1'

show2D :: Bitboard -> String
show2D b = unlines . reverse $ [take 8 $ drop n (showBin b) | n <- [0,8..56]]