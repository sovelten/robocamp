module Board where

import System.Random
import System.Random.Shuffle
import Utils
import Data.Char
import Data.Maybe

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

data Move = Move RobotPlayer Pos Pos
    deriving (Show)

opponent :: RobotPlayer -> RobotPlayer
opponent A = B
opponent B = A

--Pretty printing
moveToStr :: Move -> String
moveToStr (Move p (x1,y1) (x2,y2)) = unwords [show p,show x1,show y1,show x2,show y2]

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

inside :: (Int,Int) -> Pos -> Bool
inside (m,n) (x,y) = x >= 0 && x < m && y >= 0 && y < n

west (x,y) = (x,y-1)
east (x,y) = (x,y+1)
north (x,y) = (x-1,y)
south (x,y) = (x+1,y)

bounds :: [[a]] -> (Int,Int)
bounds b = (length b, length (head b))

insideBoard :: Board -> (Int, Int) -> Bool
insideBoard = inside . bounds

--Generate random board

--Divide uma lista em listas de n elementos
divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n list = take n list : divide n (drop n list)

genResources :: RandomGen g => g -> Int -> [Resource]
genResources g n = map Resource randList
    where
        randList = take n (randomRs (1,9) g :: [Int])

genNiceBoard :: IO Board
genNiceBoard = do
    g <- getStdGen
    let board = genBoard g
    if isNiceBoard board
        then return board
        else genNiceBoard

isStuck :: Board -> Pos -> Bool
isStuck board p = null $ availableMove board p

isNiceBoard :: Board -> Bool
isNiceBoard board = all (not . isStuck board) robots
    where
        robots = getRobots A board ++ getRobots B board

availableMove :: Board -> Pos -> [(Pos,Pos)]
availableMove board p1 = [(p1,q) | q <- map snd sqrPosTuple]
    where
        newPosList = filter (insideBoard board) (map ($p1) [west, east, north, south])
        sqrPosTuple = filter (isEmpty . fst) (zip (map ((flip fromPos) board) newPosList) newPosList)

isEmpty :: Square -> Bool
isEmpty = (== Empty)

genBoard :: RandomGen g => g -> Board
genBoard g = divide n (shuffle' squareList (m*n) g5)
    where
        (m,g1) = randomR (10,20::Int) g
        (n,g2) = randomR (10,20) g1
        (r,g3) = randomR (2,6) g2
        (x,g4) = randomR (5,25) g3
        (k,g5) = randomR (2,10) g4
        resources = genResources g5 k
        squareList = replicate r (R (Robot A 1)) ++
                     replicate r (R (Robot B 1)) ++
                     replicate x Wall ++
                     map E resources ++
                     replicate (m*n - 2*r - x - k) Empty

genBoard2 :: RandomGen g => g -> Board
genBoard2 g = divide n squareList
    where
        (m,g1) = randomR (10,20::Int) g
        (n,g2) = randomR (10,20) g1
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

--Tests whether a square contains a robot controlled by a player
isFromPlayer :: RobotPlayer -> Square -> Bool
isFromPlayer p (R (Robot q _)) = p == q
isFromPlayer _ __ = False

--Checks if a move is valid
isValid :: RobotPlayer -> (Pos,Pos) -> Board -> Bool
isValid pl (p1,p2) board = isFromPlayer pl piece && isAdjacent && isInside
    where piece = fromPos p1 board
          isAdjacent = any (==p2) (map ($p1) [west,east,north,south])
          isInside = insideBoard board p2

--Defines result of a movement
encounter :: Robot -> Square -> Maybe Square
encounter r s = case s of
    (E (Resource i)) -> (Just (R (Robot p (e+i))))
    (R (Robot q f)) ->
        if e > f then (Just (R (Robot p (e-f))))
        else if f > e then (Just (R (Robot q (f-e))))
        else Just Empty
    (Empty) -> (Just (R r))
    _ -> Nothing
    where
        p = robotPlayer r
        e = robotEnergy r

--returns new board if move is valid
boardMove :: Move -> Board -> Maybe Board
boardMove (Move pl p1 p2) board =
    if isValid pl (p1,p2) board 
        then if isJust newPiece
            then Just $ updateMatrix p2 (fromJust newPiece) (updateMatrix p1 (Empty) board)
            else Nothing
    else Nothing
    where
        robot = fromSquare $ fromPos p1 board
        newPiece = encounter robot (fromPos p2 board)
         
--Will fail if Square is not a Robot
fromSquare :: Square -> Robot
fromSquare s = case s of
    (R r) -> r
    _ -> error "Square is not a Robot"

--Returns the positions of all the robots from a player
getRobots :: RobotPlayer -> Board -> [Pos]
getRobots p board = findPos (isFromPlayer p) board

outOfRobots :: RobotPlayer -> Board -> Bool
outOfRobots p board = null (getRobots p board)

listRobots :: RobotPlayer -> Board -> [Robot]
listRobots p = (map fromSquare) . filter (isFromPlayer p) . concat
