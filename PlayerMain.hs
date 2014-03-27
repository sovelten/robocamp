import Player
import Board
import System.IO
import Control.Monad
import Data.Maybe

--Jogador nao interativo
--recebe tabuleiro da entrada padrao e retorna o movimento do jogador na saida padrao
main = do
    p <- fmap read getLine
    [m,n] <- fmap ((map read) . words) getLine
    board <- fmap (fromJust . readBoard) (replicateM m getLine)
    sendMove p board
