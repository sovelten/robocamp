module MDP where

import Board

distance :: Pos -> Pos
distance (x1,y1) (x2,y2) = (abs (x1-x2), abs (y1-y2))

distanceToNearest :: (Square -> Bool) -> Board -> Pos -> Int
distanceToNearest f b pos = minimum . map (distance pos) . findPos f b

--Define features for approximate q-learning
