module Stack2
  (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = S [a] Int
--INVARIANTE DE REPRESENTACION:
-- *El último elemento agregado al stack es el primer elemento de la lista y así en orden.
--Siendo el último elemento de la lista el primero en haber sido agregado al stack.
-- *El int debe siempre ser equivalente al valor del length de [a].

--O(1)
emptyS :: Stack a
emptyS = S [] 0

--O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs _) = null xs

--O(1)
push :: a -> Stack a -> Stack a
push x (S xs n) = S (x:xs) (n+1)

--O(1)
top :: Stack a -> a
--precon: Hay elemento en la pila.
top (S xs _) = head xs

--O(1)
pop :: Stack a -> Stack a
--precon: Hay elemento en la pila
pop (S xs n) = S (tail xs) (n-1)

--O(1)
lenS :: Stack a -> Int
lenS (S xs n) = n
