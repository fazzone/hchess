module Board where

import Data.Char (ord, chr)

import Direction

{-
About square representation and ordering:
Programatically, squares are represented as simple a (Int,Int).  First element of the pair is the file or 'x',
then the rank or 'y'.  (0,0) is a1 and (7,7) is h8.  
-}
type Position = (Int,Int)

squareToName :: Position -> String
squareToName (file, rank) = chr (file + ord 'a') : show (1+rank)

nameToSquare :: String -> Position
nameToSquare [cf,cr] = (ord cf - ord 'a', ord cr - ord '1')
nameToSquare _ = undefined

onChessboard :: Position -> Bool
onChessboard (file, rank) = file < 8 && file >= 0 && rank < 8 && rank >= 0

cbRay :: Position -> Direction -> [Position]
cbRay pos = takeWhile onChessboard . ray pos

