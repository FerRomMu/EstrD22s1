module Queue 
  (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
where

--De guias anteriores
agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado
--al final de la lista.
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e
---------------------

data Queue a = Q [a]
--INVARIANTE DE REPRESENTACION:
--Los elementos de la lista deben estar ordenados por llegada de manera tal que
--el primero de la lista sea el primero en haber llegado y así con el resto en orden
--siendo el último en llegar el último de la lista.

--O(1)
emptyQ :: Queue a
emptyQ = Q []

--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

--O(n)
queue :: a -> Queue a -> Queue a
queue x (Q xs) = Q (agregarAlFinal xs x)

--O(1)
--precon: Hay elemento en la fila.
firstQ :: Queue a -> a
firstQ (Q xs) = head xs

--O(1)
--precon: Hay elemento en la fila.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (tail xs)
