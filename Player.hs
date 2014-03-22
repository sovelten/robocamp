module Player where

import Board
import System.Random
import Utils
import Data.List
import Data.Maybe
import Control.Monad

data Direction = North | South | West | East

inside :: (Int,Int) -> Pos -> Bool
inside (m,n) (x,y) = x >= 0 && x < n && y >= 0 && y < n

west (x,y) = (x-1,y)
east (x,y) = (x+1,y)
north (x,y) = (x,y-1)
south (x,y) = (x,y+1)

type Move = (Pos, Pos)

getRobots :: RobotPlayer -> Board -> [Pos]
getRobots p board = findPos (isPlayer p) board

--Gives a list of positions of all the elements matching the predicate
findPos :: (a -> Bool) -> [[a]] -> [Pos]
findPos f board = findAux 0 board
    where findAux i (x:xs) = (map (\x -> (i,x)) (findIndices f x)) ++ (findAux (i+1) xs)
          findAux _ [] = []

action :: RobotPlayer -> Board -> (Move,Board)
action p board = ((p1,p2), (updateMatrix p2 (fromPos p1 board) board))
    where robotsPos = getRobots p board
          (p1,p2) = moveRobot board robotsPos

moveRobot :: Board -> [Pos] -> Move
moveRobot board points = head (concatMap (availableMove board) points)

availableMove :: Board -> Pos -> [Move]
availableMove board p1 = [(p1,q) | q <- map snd sqrPosTuple]
    where
        bnds = bounds board
        newPosList = filter (inside bnds) (map ($p1) [west, east, north, south])
        sqrPosTuple = filter (isEmpty . fst) (zip (map ((flip fromPos) board) newPosList) newPosList)

bounds :: Board -> (Int,Int)
bounds b = (length b, length (head b))

isEmpty :: Square -> Bool
isEmpty = (== Empty)

command :: RobotPlayer -> Move -> String
command p ((x1,y1),(x2,y2)) = unwords [show p,show x1,show y1,show x2,show y2]

readCmd :: String -> (RobotPlayer, Move)
readCmd str = (p, ((x1,y1),(x2,y2)))
    where input = words str
          p = read (head input)
          [x1,y1,x2,y2] = map read (tail input)

sendMove :: RobotPlayer -> Board -> IO Board
sendMove p board = do
    let (m,newBoard) = action p board  
    putStrLn $ command p m
    return newBoard 

recvMove :: Board -> IO Board
recvMove board = do
    (_,(p1,p2)) <- fmap (readCmd) getLine
    let newBoard = updateMatrix p2 (fromPos p1 board) board
    return newBoard

interaction :: RobotPlayer -> Board -> IO ()
interaction p board = do
    board1 <- recvMove board
    board2 <- sendMove p board1
    interaction p board2

main2 = do
    g <- getStdGen
    let board = genBoard g
    let p = A
    newboard <- sendMove p board
    interaction p board
    --return ()
    
main3 = do
    p <- fmap read getLine
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    init <- getLine
    if init == "start" then do
        board <- sendMove p board
        board <- recvMove board
        board <- sendMove p board
        return ()
        --interaction p board
    else do
        interaction p board
