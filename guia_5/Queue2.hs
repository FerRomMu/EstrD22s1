module Queue2 
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
--Los elementos de la lista deben estar ordenados por llegada de manerta tal que
--el primero de la lista sea el último en haber llegado y así con el resto en orden
--siendl el último en llegar el primero de la lista.

--O(1)
emptyQ :: Queue a
emptyQ = Q []

--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

--O(1)
queue :: a -> Queue a -> Queue a
queue x (Q xs) = Q (x:xs)

--O(n?)
--precon: Hay elemento en la fila
firstQ :: Queue a -> a
firstQ (Q xs) = last xs

--O(n)
dequeue :: Queue a -> Queue a
--precon: Hay elemento en la fila.
dequeue (Q xs) = Q (init xs)
