import Stack2

--Costo O(n)
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

--Costo O(n)
desapilar :: Stack a -> [a]
desapilar s =
  if (isEmptyS s)
    then []
    else (top s) : (desapilar  (pop s))

--Costo O(n)
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s
insertarEnPos n x s =
  if(isEmptyS s)
    then error "No hay posicion dada."
    else push (top s) (insertarEnPos (n-1) x (pop s))
