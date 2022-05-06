import Map3
import MyJust

--O(n*k) <- donde n*k es el costo de valueMInKeys
valueM :: Eq k => Map k v -> [Maybe v]
valueM m = valueMInKeys m (keys m)

--O(n*k) donde n es el costo de lookupM y k es el largo de la lista dada.
valueMInKeys :: Eq k => Map k v -> [k] -> [Maybe v]
valueMInKeys m [] = []
valueMInKeys m (x:xs) = (lookupM x m) : valueMInKeys m xs

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks m = contieneA ks (keys m)

--O(n*m) <- donde n es el largo de la primer lista dada y m el largo de la segunda
--lista dada (en realidad el costo de elem x ys)
contieneA :: Eq a => [a] -> [a] -> Bool
--Devuelve todos los elementos del primero estan contenidos en el segundo.
contieneA [] ys = True
contieneA (x:xs) ys = (elem x ys) && contieneA xs ys

--O(n) <- donde n es el largo de la lista dada.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,x):xs) = assocM k x (listToMap xs)

--O(k) <- donde k es la cantidad de keys que tiene el map dado.
mapToList :: Eq k => Map k v -> [(k,v)]
mapToList m = mapToListKeys m (keys m)

--O(k) <-donde k es la cantidad de elementos de la lista de keys.
mapToListKeys :: Eq k => Map k v -> [k] -> [(k,v)]
mapToListKeys m [] = []
mapToListKeys m (x:xs) = (x, fromJust(lookupM x m)): mapToListKeys m xs

--O(m) <- donde m es el costo de addElemToListInMap
agruparEq :: Eq k => [(k,v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,x):xs) = addElemToListInMap k x (agruparEq xs)

--O(m) <- donde m es el costo de hacer lookupM c m o de hacer assocM c x m
addElemToListInMap :: Eq k => k -> a -> Map k [a] -> Map k [a]
addElemToListInMap c x m =
  let
    listK = fromMaybe [] (lookupM c m)
  in
    assocM c (x:listK) m

--O(k*m) <-- donde k es la cantidad de elementos de la lista de keys y m
--es el costo de incrementarElDeKey
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--precon: Hay un valor asociado en el map a cada key de la lista dada.
incrementar [] m = m
incrementar (x:xs) m = incrementarElDeKey x (incrementar xs m)

--O(m) <- donde m es el costo de hacer lookupM x m o de hacer assocM x i m
incrementarElDeKey :: Eq k => k -> Map k Int -> Map k Int
incrementarElDeKey x m =
  let
    aIncrementar = fromJust(lookupM x m)
  in
    assocM x (aIncrementar + 1) m

--O(k*m) donde k*m es el costo de mergeMapsWithKey
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m n = mergeMapsWithKey (keys m) m n

--O(k*m) donde k es la cantidad de elementos de la lista de keys
-- y m es el costo de lookupM del map m o assocM del map n
mergeMapsWithKey :: Eq k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsWithKey [] m n = n
mergeMapsWithKey (x:xs) m n =
  let
    valueMerge = fromJust(lookupM x m)
  in
    assocM x valueMerge (mergeMapsWithKey xs m n)

