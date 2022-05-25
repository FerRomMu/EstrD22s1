data RAList = Mkr Int (Map Int a) (Heap a)

--INVIARIANTE DE REPRESENTACION:
-- * El Int siempre debe valer igual al número correspondiente a la proxima
--posicion a ocupar en la lista. Ademas, si la estructura esta vacía debe
--valer 0.
-- * Los elementos de la heap y los valores del map deben ser los mismos 
--(aunque puedan estar en otro orden).
-- * Si el Int es 0, tanto el Map como el Heap estan vacios.

--ACLARACION SOBRE COSTOS: Para todos los costos N se refiere a la cantidad
--de elementos en la RAList.

emptyRAL :: RAList a
emptyRAL = Mkr 0 emptyM emptyH
--O(1)
--JUstificacion: Todas las funciones usadas son O(1).

isEmptyRAL :: RAList a -> Bool
isEmptyRAL (Mkr n m h) = n == 0
--O(1)
--JUSTIFICACION: Solo se realiza un pattern matching y una operacion logica.
--Ambas O(1).

lengthRAL :: RAList a -> Int
lengthRAl (Mkr n m h) = n
--O(1)
--JUSTIFICACION: Solo se hace pattern matching.

get :: Int -> RAList a -> a
get i (Mkr n m h) =
  case (lookupM i m) of
    Nothing -> error "No hay elemento con indice dado."
    Just v -> v
--O(log N)
--Justificacion: Se utiliza lookupM sobre el map de elementos lo que es
--costo log (N).

minRAL :: Ord a => RAList a -> a
minRAL (Mkr n m h) = findMin h
--O(1)
--JUSTIFICACION: Se utiliza pattern matching y findMin sobre el heap.

add :: Ord a => a -> RAList a -> RAList a
add v (Mkr n m h) = Mkr (n+1) (assocM n v m) (insertH v h)
--O(log N)
--JUSTIFICACION: Se utiliza tanto assocM como insertH, ambos O(log N),
--las demas operaciones son despreciables por ser O(1).

elems :: Ord a => RAList a -> [a]
elems (Mkr n m h) = todosDesde 0 n m

todosDesde :: Ord a => Int -> Int -> Map Int a -> [a]
todosDesde n m =
  if (i < n)
    then valor : (todosDesde (i+1) n m)
    else []
  where
    valor = fromJust (lookupM i m)
--O(N * log N)
--Justificacion: Por cada elemento hago un lookupM de O(log N).

remove :: Ord a => RAList -> RAList a
remove (Mkr n m h) = Mkr (n-1) (deleteM (n-1) m) (heapWithOut ultimo h)
  where ultimo = fromJust (lookupM (n-1) m)
--O(N*logN)
--Justficacion: Dado que heapWithOut es O(N*log N) y las demas funciones
--son despreciables por ser de costo menor (por ej lookupM costo log N).

heapWithOut :: Ord a => a -> Heap a
heapWithOut n h = if (n == findMin h)
                then deleteMin h
                else insertH (findMin h) (heapWithOut n (deleteMin h))
--O(N*log N)
--JUSTIFICACION: Se puede llegar a recorrer cada elemento del heap y por cada
--uno se realiaza deleteMin que es O(log N).

set :: Ord a => Int -> a -> RAList -> RAList a
set n v (Mkr n m h) = Mkr n (assocM i v m) (reemplazar v viejo h)
  where viejo = fromJust (lookupM i m)
--O(N*logN)
--JUSTIFICACION: Reemplazar es costo N*log N, al ser los demas costes 
--menores esos son despreciables.

reemplazar :: Ord a => a -> a -> Heap a
reemplazar n v h = 
  if (k == findMin h)
    then insertH n (deleteMin h)
    else reemplazar n v (deleteMin h)
--O(N*LogN)
--JUSTIFICACION: Se recorrera en el peor caso los N elementos y por cada
--elemento recorrido se hace funciones O(logN), como deleteMin b.

addN :: Ord a => Int -> a -> RAList a -> RAList a
addN i v (Mkr n m h) = Mkr (n+1) (addIn i v m) (insertH v h)
--O(N*logN)
--Justificacion: Todas las funciones estan en funcion de N, al ser addIn
--la de mayor coste, el costo total es O(N*LogN)

addIn :: Ord a => Int -> a -> Map Int a -> MapInt a
addIn i v m =
  case (lookupM i m) of
    Nothing -> assocm i v m
    Just e -> assocM i v (addIn (i+1) e m)
--O(n*logN) en el peor caso (donde i es 0) se recorrera todos los elementos
--del map y por cada uno se hace assocM que es O(log N).
