import Board
import Control.Monad
import Data.Maybe (isJust, fromJust)
import Game
import System.Environment (getArgs)
import System.IO
import System.Process (runInteractiveCommand)
    
main = do
    [pa,pb] <- getArgs
    (hinA,houtA,herrA,pA) <- (runInteractiveCommand ("./"++pa))
    (hinB,houtB,herrB,pB) <- (runInteractiveCommand ("./"++pb))
    mapM_ (flip hSetBinaryMode False) [hinA,hinB,houtA,houtB]
    mapM_ (flip hSetBuffering LineBuffering) [hinA,hinB]
    mapM_ (flip hSetBuffering NoBuffering) [houtA,houtB]
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    let str = show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board)
    hPutStr hinA ("A\n" ++ str)
    hPutStr hinB ("B\n" ++ str)
    putStr str
    gs <- firstRound houtA (GameState board 0 Nothing []) 
    if (isJust $ victory gs)
        then do
            print $ fromJust $ victory gs
        else do
            endGs <- rounds ((hinA,houtA),(hinB,houtB)) gs 
            putStr (unlines (map moveToStr (reverse $ moveLog endGs)))
            if (isJust $ victory endGs)
                then putStrLn (snd (fromJust $ victory endGs))
                else do
                    let win = checkWinner (gameBoard endGs)
                    if (isJust win)
                        then putStrLn (show (fromJust win) ++ " venceu!") 
                        else putStrLn "Empate"
