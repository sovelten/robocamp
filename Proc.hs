import System.Process
import System.IO
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Random
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Board

parseUntilPrompt :: Handle -> IO [String]                    
parseUntilPrompt out = do                                    
    latest <- hGetLine out                                     
    putStrLn latest
    if latest == ""                                            
        then return []                                           
        else (:) <$> return latest <*> parseUntilPrompt out

main = do
    (hin,hout,herr,pl) <- (runInteractiveCommand "./Player")
    hSetBinaryMode hin False
    hSetBinaryMode hout False
    hSetBuffering hin LineBuffering
    hSetBuffering hout NoBuffering
    g <- getStdGen
    let board = genBoard g
    let (m,n) = (length board, length (head board))
    let str = "A\n" ++ show m ++ " " ++ show n ++ "\n" ++ (prettyBoard board) ++ "start\n"
    TIO.hPutStr hin (T.pack str)
    out <- TIO.hGetLine hout 
    TIO.hPutStr hin $ T.pack "A 0 0 0 1\n"
    TIO.putStrLn out
    out2 <- TIO.hGetLine hout
    TIO.putStrLn out2
