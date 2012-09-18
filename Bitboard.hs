module Bitboard (
        Position, squareToName, nameToSquare, onChessboard,
        Bitboard, flattenSq, unflattenSq, toBitboard, showBin, show2D) where

import Data.Char (ord, chr)
import Data.Bits ((.&.), (.|.), shiftL, bit)
import Data.List(foldl')
import Data.Word (Word64)


--First some stuff about positions
--About square representation and ordering:
--Programatically, squares are represented as simple a (Int,Int).  First element of the pair is the file or 'x',
--then the rank or 'y'.  (0,0) is a1 and (7,7) is h8.
type Position = (Int,Int)

squareToName :: Position -> String
squareToName (file, rank) = chr (file + ord 'a') : show (1+rank)

nameToSquare :: String -> Position
nameToSquare [cf,cr] = (ord cf - ord 'a', ord cr - ord '1')
nameToSquare _ = undefined

onChessboard :: Position -> Bool
onChessboard (file, rank) = file < 8 && file >= 0 && rank < 8 && rank >= 0


--Now some bitboard stuff
type Bitboard = Word64

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