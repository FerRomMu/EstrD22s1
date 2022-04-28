module Queue3
  (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
where

--De guias viejas:

--O(n)
agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado
--al final de la lista.
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

--O(n²)
reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para
--adelante.
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x
------------------
data Queue a = Q [a] [a]
{--fs es la primer lista, bs es la segunda, quedando de modo: Q fs bs
  Invariante de representación:
  * Siempre que fs sea vacía, bs debe ser vacía, estando la cola vacía.
--}

--O(1)
emptyQ :: Queue a
emptyQ = Q [] []

--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs bs) = null fs

--O(1)
queue :: a -> Queue a -> Queue a
queue x (Q fs bs) = queueOrdenada (Q fs (x:bs))

--O(1)
firstQ :: Queue a -> a
firstQ (Q fs bs) = head fs

--O(1)
--Cuando fs vacia -> O(n²)
dequeue :: Queue a -> Queue a
dequeue (Q fs bs) = queueOrdenada (Q (tail fs) bs)

--O(1)
--Cuando fs vacia -> O(n²)
queueOrdenada :: Queue a -> Queue a
queueOrdenada (Q fs bs) =
  if (null fs)
    then Q (reversa(bs)) []
    else Q fs bs
