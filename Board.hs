module Board where

import System.Random
import System.Random.Shuffle
import Utils
import Data.Char

--Board definition
type Board = [[Square]]

--Position
type Pos = (Int,Int)

--Square definition
data Square = Empty | Wall | E Resource | R Robot
    deriving (Show,Eq)

--Robot definition
data Robot = Robot {robotPlayer::RobotPlayer,robotEnergy::Int}
    deriving (Show,Eq)
data RobotPlayer = A | B deriving (Read, Show, Eq)

--Resource definition
data Resource = Resource Int
    deriving (Show, Eq)

--Pretty printing
prettyBoard :: Board -> String
prettyBoard = unlines . map (concatMap prettySquare)

prettySquare :: Square -> String
prettySquare Empty = ".."
prettySquare Wall  = "XX"
prettySquare (R (Robot p e)) = show p ++ show e
prettySquare (E (Resource e)) = "R" ++ show e

readBoard :: [String] -> Maybe Board
readBoard = sequence . (map (sequence . readBoardLine))

readBoardLine :: String -> [Maybe Square]
readBoardLine (x:xs)
    | any (x==) ['A','B','R'] = (readSquare (x:num)):(readBoardLine rest)
    | otherwise = (readSquare [x, head xs]) : (readBoardLine (tail xs))
        where num = takeWhile isDigit xs
              rest = dropWhile isDigit xs
readBoardLine [] = []

readSquare :: String -> Maybe Square
readSquare ".." = (Just Empty)
readSquare "XX" = (Just Wall)
readSquare (x:xs)
    | any (x==) ['A','B']  = (Just (R (Robot (read [x]) (read xs))))
    | x == 'R' = (Just (E (Resource (read xs))))
    | otherwise = Nothing
    

--Generate random board

--Divide uma lista em listas de n elementos
divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n list = take n list : divide n (drop n list)

genBoard :: RandomGen g => g -> Board
genBoard g = divide n (shuffle' squareList (m*n) g5)
    where
        (m,g1) = randomR (10,30::Int) g
        (n,g2) = randomR (10,30) g1
        (r,g3) = randomR (2,6) g2
        (x,g4) = randomR (10,30) g3
        (k,g5) = randomR (2,12) g4
        squareList = replicate r (R (Robot A 1)) ++
                     replicate r (R (Robot B 1)) ++
                     replicate x Wall ++
                     replicate k (E (Resource 1)) ++
                     replicate (m*n - 2*r - x - k) Empty

genBoard2 :: RandomGen g => g -> Board
genBoard2 g = divide n squareList
    where
        (m,g1) = randomR (10,30::Int) g
        (n,g2) = randomR (10,30) g1
        randList = take (m*n) (randomRs (0,1) g2 :: [Double])
        squareList = map (genSquare) randList 

genSquare :: Double -> Square
genSquare f
    | f < 0.2   = Wall
    | f < 0.5   = E (Resource 1)
    | f < 0.9   = Empty
    | otherwise = R (Robot A 1)

fromPos :: Pos -> Board -> Square
fromPos (r,c) board = board !! r !! c

isRobot :: Square -> Bool
isRobot r = case r of
    (R _) -> True
    _ -> False

--Checks if a move is valid
isValid :: (Pos,Pos) -> Board -> Bool
isValid (p1,p2) board = (isRobot piece) && (dest == Empty)
    where piece = fromPos p1 board
          dest  = fromPos p2 board

--returns new board if move is valid
move :: (Pos,Pos) -> Board -> Maybe Board
move (p1,p2) board = if isValid (p1,p2) board 
    then Just $ updateMatrix p2 (fromPos p1 board) (updateMatrix p1 (Empty) board)
    else Nothing

isPlayer :: RobotPlayer -> Square -> Bool
isPlayer p (R (Robot b _)) = p == b
isPlayer _ _ = False
