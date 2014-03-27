import Control.Monad
import System.Process
import System.IO
import System.Random
import Board

main = do
    (hinA,houtA,herrA,pA) <- (runInteractiveCommand "./PlayerInteractive")
    (hinB,houtB,herrB,pB) <- (runInteractiveCommand "./PlayerInteractive")
    mapM_ (flip hSetBinaryMode False) [hinA,hinB,houtA,houtB]
    mapM_ (flip hSetBuffering LineBuffering) [hinA,hinB]
    mapM_ (flip hSetBuffering NoBuffering) [houtA,houtB]
    g <- getStdGen
    let board = genBoard g
    let (m,n) = (length board, length (head board))
    let str = show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board)
    hPutStr hinA ("A\n" ++ str)
    hPutStr hinB ("B\n" ++ str)
    fMove <- hGetLine houtA
    putStrLn fMove
    hPutStrLn hinB fMove
    sMove <- hGetLine houtB
    putStrLn sMove
