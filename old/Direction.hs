module Direction where

{-
Directions, with a1 in the bottom left:
East:	increasing x
South:	decreasing y
West:	decreasing x
North:	increasing y
-}

data Direction = East | Southeast | South | Southwest | West | Northwest | North | Northeast deriving (Eq, Show, Enum)

asIncrement :: Direction -> (Int, Int)
asIncrement dir = case dir of
  East		->	(1,0)
  Southeast	->	(1,-1)
  South		->	(0,-1)
  Southwest	->	(-1,-1)
  West		->	(-1, 0)
  Northwest	->	(-1, 1)
  North		->	(0,1)
  Northeast	->	(1,1)
  
ray :: (Int, Int) -> Direction -> [(Int, Int)]
ray (x,y) dir = let
  (xi, yi) = asIncrement dir
  nps = (x+xi, y+yi)
  in nps : ray nps dir
     
