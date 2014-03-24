module Utils where

addPair::(Int,Int)->(Int,Int)->(Int,Int)
addPair (a,b) (c,d) = (a+c,b+d)
multPair::Int->(Int,Int)->(Int,Int)
multPair n (a,b) = (n*a,n*b)

applyAll::a->[a->a]->a
applyAll a [] = a
applyAll a (f:xs) = applyAll (f a) xs

updateList::[a]->Int->(a->a)->[a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f

updateMatrix::(Int, Int)->a->[[a]]->[[a]]
updateMatrix (i,j) a m = updateList m i (\z->updateList z j (const a)) 

