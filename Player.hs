module Player where

import Board
import System.Random
import Utils
import Data.List
import Data.Maybe
import Control.Monad

getRobots :: RobotPlayer -> Board -> [Pos]
getRobots p board = findPos (isFromPlayer p) board

--Gives a list of positions of all the elements matching the predicate
findPos :: (a -> Bool) -> [[a]] -> [Pos]
findPos f board = findAux 0 board
    where findAux i (x:xs) = (map (\x -> (i,x)) (findIndices f x)) ++ (findAux (i+1) xs)
          findAux _ [] = []

action :: RobotPlayer -> Board -> (Move,Board)
action p board = ((Move p p1 p2), (fromJust $ boardMove (Move p p1 p2) board))
    where robotsPos = getRobots p board
          (p1,p2) = moveRobot board robotsPos

moveRobot :: Board -> [Pos] -> (Pos,Pos)
moveRobot board points = head (concatMap (availableMove board) points)

availableMove :: Board -> Pos -> [(Pos,Pos)]
availableMove board p1 = [(p1,q) | q <- map snd sqrPosTuple]
    where
        newPosList = filter (insideBoard board) (map ($p1) [west, east, north, south])
        sqrPosTuple = filter (isEmpty . fst) (zip (map ((flip fromPos) board) newPosList) newPosList)

isEmpty :: Square -> Bool
isEmpty = (== Empty)

command :: Move -> String
command (Move p (x1,y1) (x2,y2)) = unwords [show p,show x1,show y1,show x2,show y2]

readCmd :: String -> Move
readCmd str = (Move p (x1,y1) (x2,y2))
    where input = words str
          p = read (head input)
          [x1,y1,x2,y2] = map read (tail input)

sendMove :: RobotPlayer -> Board -> IO Board
sendMove p board = do
    let (move,newBoard) = action p board  
    putStrLn $ command move
    return newBoard 

recvMove :: Board -> IO Board
recvMove board = do
    move <- fmap (readCmd) getLine
    let newBoard = fromJust $ boardMove move board
    return newBoard

interaction :: RobotPlayer -> Board -> IO ()
interaction p board = do
    board1 <- recvMove board
    board2 <- sendMove p board1
    interaction p board2
