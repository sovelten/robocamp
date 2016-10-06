import Board
import Control.Monad  
import Data.Maybe
import Game
import Text.Read (readMaybe)

readMove :: String -> Maybe Move
readMove str = if isJust p && all isJust [x1,y1,x2,y2] then
        (Just (Move (fromJust p) (fromJust x1, fromJust y1) (fromJust x2, fromJust y2)))
        else Nothing
    where input = words str 
          p = readMaybe (head input)
          [x1,y1,x2,y2] = map (readMaybe) (tail input)

foldMove :: [Move] -> Board -> [Board]
foldMove (m:ms) b = newboard : (foldMove ms newboard)
    where nb = boardMove m b
          newboard = if isJust nb then fromJust nb else b

foldMove [] _ = []

printRound :: (Int,(Move,Board)) -> IO () 
printRound (i,(m,b)) = do
    putStrLn ("Turn " ++ (show i) ++ ": " ++ (moveToStr m))
    putStrLn $ prettyBoard b

main = do
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    lines <- fmap lines getContents 
    let moves = catMaybes (map (readMove) lines) :: [Move]
    let boards = foldMove moves board
    let zboards = zip [1..] (zip moves boards)
    sequence_ (map printRound zboards) 
    --let (Move pl p1 p2) = last moves
    --putStrLn $ show  $ checkWinner (last boards)
    --putStr $ prettyBoard $ fromJust $ boardMove (Move pl p1 p2) (last (init boards))
