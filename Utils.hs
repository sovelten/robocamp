module Utils where

import Data.List

updateList::[a]->Int->(a->a)->[a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f

updateMatrix::(Int, Int)->a->[[a]]->[[a]]
updateMatrix (i,j) a m = updateList m i (\z->updateList z j (const a)) 

--Gives a list of positions of all the elements matching the predicate
findPos :: (a -> Bool) -> [[a]] -> [(Int,Int)]
findPos f board = findAux 0 board
    where findAux i (x:xs) = (map (\x -> (i,x)) (findIndices f x)) ++ (findAux (i+1) xs)
          findAux _ [] = []
