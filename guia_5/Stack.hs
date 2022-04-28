module Stack
  (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = S [a]
--INVARIANTE DE REPRESENTACION:
--El último elemento agregado al stack es el primer elemento de la lista y así en orden.
--Siendo el último elemento de la lista el primero en haber sido agregado al stack.

--O(1)
emptyS :: Stack a
emptyS = S []

--O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs) = null xs

--O(1)
push :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)

--O(1)
top :: Stack a -> a
--precon: Hay elemento en la pila.
top (S xs) = head xs

--O(1)
pop :: Stack a -> Stack a
--precon: Hay elemento en la pila
pop (S xs) = S (tail xs)

--O(n)
lenS :: Stack a -> Int
lenS (S xs) = length xs
