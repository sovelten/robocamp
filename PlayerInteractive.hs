import Player
import Board
import System.IO
import Control.Monad
import Data.Maybe

main = do
    hSetBuffering stdout LineBuffering
    p <- fmap read getLine
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    if p == A then do
        board1 <- sendMove p board
        interaction p board1
    else do
        interaction p board
