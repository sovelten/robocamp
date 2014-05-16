import Board
import System.Random

main = do
    board <- genNiceBoard
    --let board = genBoard g
    let (m,n) = bounds board
    putStrLn ((show m) ++ " " ++ (show n))
    putStr (prettyBoard board)
