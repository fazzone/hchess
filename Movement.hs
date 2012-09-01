module Movement (
  module Direction, module Board, Piece(..), 
  getRelevantOccupancy, getPossibleROs, getAttackSets, getAttackSetFor
  ) where

import Control.Monad (filterM)
import Data.List (inits)

import Direction
import Board

--only pieces we need to worry about right now
data Piece = Bishop | Rook deriving (Eq, Show, Read)

movementAxes :: Piece -> [Direction]
movementAxes Rook	= [East, South, West, North]
movementAxes Bishop	= [Southeast, Southwest, Northwest, Northeast]

getMovementRays :: Piece -> Position -> [[Position]]
getMovementRays piece pos = map (cbRay pos) (movementAxes piece)

{-
"Relevant occupancy" for a given piece at a given position is the set of squares that need to be checked to fully
determine that piece's attack set (the set of squares to which it can move).  Thus, we can save space in our table
by not including the final point on the ray-of-movement; because it doesn't change anything (the piece stops there
anyway because the board ends) 
-}
--init' to gracefully handle the case of the 0-length movement ray in a particular direction
getRelevantOccupancy :: Piece -> Position -> [Position]
getRelevantOccupancy piece pos	= concatMap init' (getMovementRays piece pos)
  where init' [] = []
        init' xs = init xs
  
--The set of all possible relevant occupancy masks is simply the powerset of the relevant occupancy square-set:
--each of those squares is either occupied (included; True) or unoccupied (left out; False)        
getPossibleROs :: Piece -> Position -> [[Position]]
getPossibleROs piece  = powerset . getRelevantOccupancy piece
  where powerset = filterM (const [True, False])

{-
In the list monad, '<-' essentially means "choose from".
so sequence, which basically a sort of fold using (<-), chooses each possible blocked-set in turn (it chooses one
blocked-set for each ray).  the filter is required because if sequence encounters a [], the whole result is [].
map concat removes an extra level of indirection from the middle (the result of the sequence operation is a list
of a bunch of 1-length lists containing the positions we want).

(tail . inits) instead of (inits) because we can always move to the first blocked square (by capturing the blocking piece)
-}
getAttackSets :: Piece -> Position -> [[Position]]
getAttackSets piece =  concat . sequence . filter (not . null) . map (tail . inits) . getMovementRays piece

--used in getAttackSetFor; we want an 'inclusive' version of takeWhile because we intuitively want to do something like
--	movesAlongRay = takeWhile (not . occupied) rayPoints
--but, we need to include the first occupied point, because we can capture the piece there.
takeWhileInclusive :: (a -> Bool) ->  [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive f (a:as)
  | f a = a : takeWhileInclusive f as
  | otherwise = [a]	--[a] instead of [] makes it inclusive

--attack set for a particular piece at a particular position given an occupancy
getAttackSetFor :: Piece -> Position -> [Position] -> [Position]
getAttackSetFor piece pos occ = concatMap (takeWhileInclusive (`notElem` occ)) . map (cbRay pos) $ movementAxes piece
