import Set

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unSet = addS 5 (addS 2 (addS 1 (emptyS)))
otroSet = addS 2 (addS 3 (addS 4 (emptyS)))

--O(n²) <-con set2
--O(n²) <-con set
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = 
  if(belongs x s)
    then x : losQuePertenecen xs s
    else losQuePertenecen xs s

--O(n²) <-con set2
--O(n²) <- con set
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos x = setToList(listToSet x)

--O(n²) <-con set2
--O(n) <- con set
listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

--O(n³?) <- con set2
--O(n²) <- con set
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT x ti td) =
  unionS x (unionS (unirTodos ti) (unirTodos td))
