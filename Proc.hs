import Control.Monad
import Data.Maybe
import Data.Either
import System.IO
import System.Process
import System.Random
import System.Timeout
import Text.Read
import Board

type ProcessHandles = (Handle,Handle)

type Victory = (RobotPlayer,String)

data GameState = GameState {gameBoard::Board, turnCount::Int, victory::Maybe Victory, moveLog::[Move]}

lastMove :: GameState -> Move
lastMove = head . moveLog

readCmd :: String -> Either String Move
readCmd str = if isJust p && all isJust [x1,y1,x2,y2] then
    (Right (Move (fromJust p) (fromJust x1,fromJust y1) (fromJust x2, fromJust y2)))
    else (Left "Invalid move format")
    where input = words str
          p = readMaybe (head input)
          [x1,y1,x2,y2] = map (readMaybe) (tail input)

moveToStr :: Move -> String
moveToStr (Move p (x1,y1) (x2,y2)) = unwords [show p,show x1,show y1,show x2,show y2]

listRobots :: RobotPlayer -> Board -> [Robot]
listRobots p = (map fromSquare) . filter (isFromPlayer p) . concat

checkWinner :: Board -> Maybe RobotPlayer
checkWinner board =
    if aEnergy > bEnergy
        then (Just A)
    else if bEnergy < aEnergy
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
        bEnergy = sum $ map (robotEnergy) aRobots

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither leftMsg = maybe (Left leftMsg) Right

recvMove :: Handle -> IO (Either String Move)
recvMove hout = do
    ans <- timeout 10000 $ hGetLine hout
    let move = maybeToEither "Timeout" ans
    case move of
        (Left msg) -> return (Left msg)
        (Right str) -> return $ readCmd str

sendMove :: Handle -> Move -> IO ()
sendMove hin m = hPutStrLn hin $ moveToStr m

comm :: Move -> ProcessHandles -> IO (Either String Move)
comm pMove (hin,hout) = do
    --sends previous player move
    sendMove hin pMove
    --receives player move
    recvMove hout

move :: Move -> Board -> Either String Board
move (Move pl p1 p2) board = maybeToEither message nb
    where nb = boardMove (Move pl p1 p2) board
          message = (show pl) ++ " fez um movimento invalido"
          
checkEndOfGame :: Board -> Maybe Victory
checkEndOfGame board =
    if outA then (Just (B,"B venceu")) else if outB then (Just (A, "A venceu")) else Nothing
    where
        outA = outOfRobots A board
        outB = outOfRobots B board

turn :: ProcessHandles -> GameState -> IO GameState
turn proc gs = do
    plMove <- comm (Move pl p1 p2) proc
    case plMove of
        (Right mv) -> return (update mv gs)
        (Left msg) -> return (GameState (gameBoard gs) ((turnCount gs) +1) (Just (pl,msg)) (moveLog gs))
        where (Move pl p1 p2) = lastMove gs

update :: Move -> GameState -> GameState
update (Move pl p1 p2) (GameState board i st ml) = 
    case nb of
        (Left str) -> (GameState board (i+1) (Just ((opponent pl),str)) ((Move pl p1 p2):ml))
        (Right b)  -> (GameState b (i+1) (checkEndOfGame b) ((Move pl p1 p2):ml))
        where nb = move (Move pl p1 p2) board

firstRound :: Handle -> GameState -> IO GameState
firstRound hout (GameState board i st ml) = do
    plMove <- recvMove hout
    case plMove of
        (Right mv) -> return $ update mv (GameState board i st ml)
        (Left msg) -> return (GameState board (i+1) (Just (B,msg)) ml)

rounds :: (ProcessHandles,ProcessHandles) -> GameState -> IO GameState
rounds pHandles gs = do
    newGs <- turn handles gs
    if (isJust (victory newGs)) || (turnCount newGs >= 180)
        then return newGs
    else rounds pHandles newGs
        where handles = if (pl == A) then snd pHandles else fst pHandles
              (Move pl p1 p2) = lastMove gs
    
main = do
    (hinA,houtA,herrA,pA) <- (runInteractiveCommand "./PlayerInteractive")
    (hinB,houtB,herrB,pB) <- (runInteractiveCommand "./PlayerInteractive")
    mapM_ (flip hSetBinaryMode False) [hinA,hinB,houtA,houtB]
    mapM_ (flip hSetBuffering LineBuffering) [hinA,hinB]
    mapM_ (flip hSetBuffering NoBuffering) [houtA,houtB]
    g <- getStdGen
    let board = genBoard g
    let (m,n) = bounds board
    let str = show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board)
    hPutStr hinA ("A\n" ++ str)
    hPutStr hinB ("B\n" ++ str)
    gs <- firstRound houtA (GameState board 0 Nothing []) 
    endGs <- rounds ((hinA,houtA),(hinB,houtB)) gs 
    putStr (unlines (map show (moveLog endGs)))
    if (isJust $ victory endGs)
        then putStrLn (snd (fromJust $ victory endGs))
        else do
            let win = checkWinner (gameBoard endGs)
            if (isJust win)
                then putStrLn (show (fromJust win))
                else putStrLn "Empate"
