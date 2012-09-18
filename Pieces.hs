module Pieces where

import Bitboard

--Piece type & simplest-possible descriptions of piece movement

--first the piece type
data Piece = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Enum, Show)

{- now a minor detour: Directions.  With a1 in the bottom left:
East:   increasing file
South:  decreasing rank
West:   decreasing file
North:  increasing rank -}

data Direction = East | Southeast | South | Southwest | West | Northwest | North | Northeast deriving (Eq, Show, Enum)

asIncrement :: Direction -> (Int, Int)
asIncrement dir = case dir of
  East          ->      (1,0)
  Southeast     ->      (1,-1)
  South         ->      (0,-1)
  Southwest     ->      (-1,-1)
  West          ->      (-1, 0)
  Northwest     ->      (-1, 1)
  North         ->      (0,1)
  Northeast     ->      (1,1)

ray :: Position -> Direction -> [Position]
ray pos dir = takeWhile onChessboard $ iterate (tap (+) (asIncrement dir)) pos
        where tap f (a1, b1) (a2, b2) = (f a1 a2, f b1 b2)
  
--castling & en passant?
getMovement :: Piece -> Position -> [Position]
getMovement piece pos@(f,r) = filter onChessboard $ case piece of
        Pawn   -> [(f, 1+r)]
        Rook   -> concatMap (tail' . ray pos) [East, South, West, North]
        Knight -> [(f+fi, r+ri) | fi <- [-2,-1,1,2], ri <- [-2,-1,1,2], abs fi /= abs ri]
        Bishop -> concatMap (ray pos) [Southeast, Southwest, Northwest, Northeast]
        Queen  -> getMovement Rook pos ++ getMovement Bishop pos
        King   -> [(f+fi, r+ri) | fi <- [-1..1], ri <- [-1..1], fi /= 0 || ri /= 0]
        where   tail' [] = []
                tail' xs = tail xs

getCaptures :: Piece -> Position -> [Position]
getCaptures Pawn (f,r) = filter onChessboard [(f+1, r+1), (f-1, r+1)]
getCaptures p pos = getMovement p pos 
