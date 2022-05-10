import PriorityQueue2

--O(n*p) que es el costo dado por pqToList.
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqToList(listToPQ xs)

--O(n) donde n es el largo de la lista dada.
listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

--O(n*p) donde n es la cantidad de elementos de la PQ y p el costo lineal
--de findMinPQ o de deleteMinPQ
pqToList :: Ord a => PriorityQueue a -> [a]
pqToList p = 
  if isEmptyPQ p
    then []
    else findMinPQ p : (pqToList (deleteMinPQ p))
