module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = Pq [a]
--INVARIANTE DE REPRESENTACION:
-- Los elementos de la lista estan siempre ordenados de mayor a menor prioridad.
-- (Considerando el menor elemento como el de mayor prioridad)

--O(1)
emptyPQ :: PriorityQueue a
emptyPQ = Pq []

--O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (Pq xs) = null xs

--O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (Pq xs) = Pq (insertarEnOrden x xs)

--O(n)
insertarEnOrden :: Ord a => a -> [a] -> [a]
insertarEnOrden x [] = x:[]
insertarEnOrden x (y:ys) =
  if(x < y)
    then x : ys
    else y : insertarEnOrden x ys

--O(1)
--Precon: Hay elemento en la PQ dada.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (Pq xs) = head xs

--O(1)
--Precon: Hay elemento en la PQ dada.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (Pq xs) = Pq (tail xs)
