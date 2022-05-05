module Map3
  (Map, emptyM, assocM, lookupM, deleteM, keys)
where

--data Maybe a = Nothing | Just a

data Map k a = M [k][a]
--INVARIANTE DE REPRESENTACION:
-- *Cada elemento en la lista de k es la key que se corresponde al valor 
--del elemento en la lista de a que se ubica en la misma posición.
-- *No pueden haber dos k repetidos en la lista.

--O(1)
emptyM :: Map a k
emptyM = M [][]

--O(n) --siendo n el largo de cualquiera de las listas 
--(ya que por invariante tienen el mismo largo)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM c x (M ks xs) = 
  let
    i = indexK c ks 0
  in
    M (reemplazarEn i c ks) (reemplazarEn i x xs)

--O(m) <--Siendo m el largo de la lista dada.
reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn n x [] = [x]
reemplazarEn 0 x (y:ys) = x : ys
reemplazarEn n x (y:ys) = y : reemplazarEn (n-1) x ys

--O(k) <-con k el largo de la lista de keys. 
indexK :: Eq k => k -> [k] -> Int -> Int
indexK c [] n = n+1
indexK c (x:xs) n =
  if(c == x)
    then n
    else indexK c xs (n+1)

--O(n)--siendo n el largo de cualquiera de las listas 
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM c (M ks xs) = findIndexMaybe (indexK c ks 0) xs

--O(m) <--Siendo m el largo de la lista dada.
findIndexMaybe :: Int -> [v] -> Maybe v
findIndexMaybe n [] = Nothing
findIndexMaybe 0 (x:xs) = Just x
findIndexMaybe n (x:xs) = findIndexMaybe (n-1) xs

--O(n)--siendo n el largo de cualquiera de las listas
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM c (M ks xs) =
  let
    i = indexK c ks 0
  in
    M (borrarEn i ks) (borrarEn i xs)

--O(m) <--Siendo m el largo de la lista dada.
borrarEn :: Int -> [a] -> [a]
borrarEn n [] = [] --Si se quiere hacer parcial debería dar error
borrarEn 0 (x:xs) = xs
borrarEn n (x:xs) = borrarEn (n-1) xs

--O(1)
keys :: Map k v -> [k]
keys (M ks xs) = ks
