import Map
import MyJust
{--
module MultiSet
  (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS,
  multiSetToList)
where
--}
data MultiSet a = MS (Map a Int)

--O(1)
emptyMS :: MultiSet a
emptyMS = MS (emptyM)

--O(n)<-- donde n es el largo de la lista obtenida por keys m
isEmptyMS :: MultiSet a -> Bool
isEmptyMS (MS m) = length (keys m) == 0

--O(m) <-- donde m es el coste de addOcurrences o ocurrencesMS
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x m = addOcurrences ((ocurrencesMS x m)+1) x m

--O(m) <-- donde m es el costo de assocM
addOcurrences :: Ord a => Int -> a -> MultiSet a -> MultiSet a
addOcurrences n x (MS m) = MS(assocM x n m)

--O(m) <- donde m es el costo de lookupM
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = fromMaybe 0 (lookupM x m)

--O(n*m) <-- dado por addAllFromMap
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS m) ns = addAllFromMap (keys m) m ns

--O(n*m) donde n es el largo de la lista de llaves y m el costo de
--el lookupM k ma o el de ms implicito en ocurrencesMS.
addAllFromMap :: Ord a => [a] -> Map a Int -> MultiSet a -> MultiSet a
addAllFromMap [] ma ms = ms
addAllFromMap (k:ks) ma ms =
  let
    ocurrenciasTotales = (fromJust(lookupM k ma)) + ocurrencesMS k ms
  in
    addOcurrences 
      ocurrenciasTotales
      k
      (addAllFromMap ks ma ms)

--O(l1*l2 + n*m) aclarado en las subtareas.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MS m) (MS n) = MS(interseccion (keyInter (keys m) (keys n)) m n)

--O(l1*l2) <- donde l1 es el largo de la primer lista y l2 de la segunda.
keyInter :: Eq a => [a] -> [a] -> [a]
keyInter [] ys = []
keyInter (x:xs) ys = 
  if(elem x ys)
    then x : keyInter xs ys
    else keyInter xs ys

--O(n*m) <- donde n es el largo de la lista dada y m el costo de lookupM de m1 o de m2
interseccion :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
interseccion [] m1 m2 = emptyM
interseccion (x:xs) m1 m2 = 
  let
    ocurrencias = fromJust(lookupM x m1) + fromJust(lookupM x m2)
  in
    assocM x ocurrencias (interseccion xs m1 m2)

--O(k) <-- donde k es igual a la cantidad de keys del map en el multiset.
multiSetToList :: Ord a => MultiSet a -> [(a,Int)]
multiSetToList (MS m) = mapToList m

--O(k) <-- donde k es igual a la cantidad de keys del map 
mapToList :: Ord k => Map k v -> [(k,v)]
mapToList m = toListWithKeys (keys m) m

--O(k) <-- donde k es igual a la cantidad de keys dadas 
toListWithKeys :: Ord k => [k] -> Map k v -> [(k,v)]
toListWithKeys [] m = []
toListWithKeys (x:xs) m =
  (x, fromJust(lookupM x m)) : toListWithKeys xs m

ms1 = addMS 2(addMS 4(addMS 2(addMS 2 (addMS 2 (emptyMS)))))
ms2 = addMS 3(addMS 8(addMS 5(addMS 4231(addMS 1(addMS 32(emptyMS))))))
