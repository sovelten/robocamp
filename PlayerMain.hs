import Player
import Board
import System.IO
import Control.Monad
import Data.Maybe

main = do
    p <- fmap read getLine
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    sendMove p board
