import Board
import Control.Monad
import Data.Maybe (isJust, fromJust)
import GameProlog
import System.Environment (getArgs)
import System.IO
import System.Process (runInteractiveCommand)

main = do
    [pa,pb] <- getArgs
    let playerA = (Player A pa)
    let playerB = (Player B pb)
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    let str = show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board)
    putStr str
    gs <- firstRound playerA (GameState board 0 Nothing []) 
    if (isJust $ victory gs) --essa cadeia de ifs precisa de mais elegancia
        then do
            case fromJust (victory gs) of
                (Awon, msg) -> putStrLn msg >> putStrLn "A venceu"
                (Bwon, msg) -> putStrLn msg >> putStrLn "B venceu"
                (Tie, msg) -> putStrLn msg >> putStrLn "Empate"
        else do
            endGs <- rounds (playerA,playerB) gs 
            putStr (unlines (map moveToStr (reverse $ moveLog endGs)))
            if (isJust $ victory endGs)
                then case fromJust (victory endGs) of
                    (Awon, msg) -> putStrLn msg >> putStrLn "A venceu"
                    (Bwon, msg) -> putStrLn msg >> putStrLn "B venceu"
                    (Tie, msg) -> putStrLn msg >> putStrLn "Empate"
                else do
                    let win = checkWinner (gameBoard endGs)
                    if (isJust win)
                        then putStrLn "Fim de Jogo" >> putStrLn ((show $ fromJust win) ++ " venceu")
                        else putStrLn "Fim de Jogo" >> putStrLn "Empate"
