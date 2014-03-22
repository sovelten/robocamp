import System.Process
import System.IO
import System.Random
import Board

main = do
    (hin,hout,herr,pl) <- (runInteractiveCommand "./PlayerMain")
    hSetBinaryMode hin False
    hSetBinaryMode hout False
    hSetBuffering hin LineBuffering
    hSetBuffering hout NoBuffering
    g <- getStdGen
    let board = genBoard g
    let (m,n) = (length board, length (head board))
    let str = "B\n" ++ show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board) ++ "\n"
    hPutStr hin str
    out <- hGetLine hout 
    putStrLn out
