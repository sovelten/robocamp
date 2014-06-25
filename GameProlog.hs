module GameProlog where

import Control.Monad
import Data.Maybe (isJust, fromJust)
import HSH
import System.Environment (getArgs)
import System.IO
import System.Process (runInteractiveCommand)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Board

type ProcessHandles = (Handle,Handle)

data EndGame = Awon | Bwon | Tie
    deriving (Show)

win :: RobotPlayer -> EndGame
win A = Awon
win B = Bwon

type Victory = (EndGame,String)

data GameState = GameState {gameBoard::Board, turnCount::Int, victory::Maybe Victory, moveLog::[Move]}

data Player = Player {side :: RobotPlayer, path :: FilePath}


--For debug purposes
prettyGame :: GameState -> String
prettyGame (GameState b i st _) = 
    ("Round: " ++ (show i) ++ "\n" ++
     "State: " ++ (show st) ++ "\n") ++
    (prettyBoard b) ++
    ("Victory: " ++ (show $ checkEndOfGame b)) ++ "\n\n"

lastMove :: GameState -> Move
lastMove = head . moveLog

readCmd :: String -> Either String Move
readCmd str = if isJust p && all isJust [x1,y1,x2,y2] then
    (Right (Move (fromJust p) (fromJust x1,fromJust y1) (fromJust x2, fromJust y2)))
    else (Left "Invalid move format")
    where input = words str
          p = readMaybe (head input)
          [x1,y1,x2,y2] = map (readMaybe) (tail input)

checkWinner :: Board -> Maybe RobotPlayer
checkWinner board =
    if aEnergy > bEnergy
        then (Just A)
    else if bEnergy > aEnergy
        then (Just B)
    else if length aRobots > length bRobots
        then (Just A)
    else if length aRobots < length bRobots
        then (Just B)
    else
        Nothing
    where
        aRobots = listRobots A board
        bRobots = listRobots B board
        aEnergy = sum $ map (robotEnergy) aRobots
        bEnergy = sum $ map (robotEnergy) bRobots

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither leftMsg = maybe (Left leftMsg) Right

boardInput :: Board -> RobotPlayer -> String
boardInput board p =
    (show p) ++ "\n" ++ size ++ (prettyBoard board)
    where (m, n) = bounds board
          size = (show m) ++ " " ++ (show n) ++ "\n"

getMove :: Player -> Board -> IO (Either String Move)
getMove (Player p fp) board = do
    let input = boardInput board p
    ans <- timeout 5000000 (run $ (echo input) -|- ("./" ++ fp))
    let move = maybeToEither "Timeout" ans
    case move of
        (Left msg) -> return (Left "Timeout")
        (Right str) -> return $ readCmd str

move :: Move -> Board -> Either String Board
move (Move pl p1 p2) board = maybeToEither message nb
    where nb = boardMove (Move pl p1 p2) board
          message = (show pl) ++ " fez um movimento invalido"
          
checkEndOfGame :: Board -> Maybe Victory
checkEndOfGame board =
    if outA && (not outB)
        then (Just (Bwon,"Fim de Jogo"))
        else if outB && (not outA)
            then (Just (Awon, "Fim de Jogo"))
            else if outA && outB
                then (Just (Tie, "Ops")) --gambiarra [fixed]
                else Nothing
    where
        outA = outOfRobots A board
        outB = outOfRobots B board

turn :: Player -> GameState -> IO GameState
turn player gs = do
    plMove <- getMove player (gameBoard gs)
    case plMove of
        (Right mv) -> return (update mv gs)
        (Left msg) -> return (GameState (gameBoard gs) ((turnCount gs) +1) (Just (win pl,msg)) (moveLog gs))
        where (Move pl p1 p2) = lastMove gs

update :: Move -> GameState -> GameState
update (Move pl p1 p2) (GameState board i st ml) = 
    case nb of
        (Left str) -> (GameState board (i+1) (Just ((win $ opponent pl),str)) ((Move pl p1 p2):ml))
        (Right b)  -> (GameState b (i+1) (checkEndOfGame b) ((Move pl p1 p2):ml))
        where nb = move (Move pl p1 p2) board

firstRound :: Player -> GameState -> IO GameState
firstRound player (GameState board i st ml) = do
    plMove <- getMove player board
    case plMove of
        (Right mv) -> return $ update mv (GameState board i st ml)
        (Left msg) -> return (GameState board (i+1) (Just (win B,msg)) ml)

rounds :: (Player,Player) -> GameState -> IO GameState
rounds (p1,p2) gs = do
    newGs <- turn curPlayer gs
    if (isJust (victory newGs)) || (turnCount newGs >= 180)
        then return newGs
    else rounds (p1,p2) newGs
        where curPlayer = if (pl == A) then p2 else p1
              (Move pl _ _) = lastMove gs
