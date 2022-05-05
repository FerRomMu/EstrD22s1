module PriorityQueue2
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = Pq [a]

--O(1)
emptyPQ :: PriorityQueue a
emptyPQ = Pq []

--O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (Pq xs) = null xs

--O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (Pq xs) = Pq (x:xs)

--O(n)<-- con n igual al largo de la lista dada
--Precon: Hay elemento en la PQ dada.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (Pq xs) = minimum xs

--O(n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Precon: Hay elemento en la PQ dada.
deleteMinPQ (Pq xs) = Pq (borrarMin xs)

--O(n)<-- con n igual al largo de la lista dada
borrarMin :: Ord a => [a] -> [a]
--Precon: Hay elemento en xs
borrarMin xs = borrar (minimum xs) xs

--O(n)<-- con n igual al largo de la lista dada
borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:ys) =
  if(x==y)
    then ys
    else y : borrar x ys
