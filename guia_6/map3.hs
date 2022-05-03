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

--O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM c x (M ks xs) = 
  let
    i = indexK c ks 0
  in
    M (reemplazarEn i c ks) (reemplazarEn i x xs)

--O(n)
reemplazarEn :: Int -> a -> [a] -> [a]
reemplazarEn n x [] = [x]
reemplazarEn 0 x (y:ys) = x : ys
reemplazarEn n x (y:ys) = y : reemplazarEn (n-1) x ys

--O(n)
indexK :: Eq k => k -> [k] -> Int -> Int
indexK c [] n = n+1
indexK c (x:xs) n =
  if(c == x)
    then n
    else indexK c xs (n+1)

--O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM c (M ks xs) = findIndexMaybe (indexK c ks 0) xs

--O(n)
findIndexMaybe :: Int -> [v] -> Maybe v
findIndexMaybe n [] = Nothing
findIndexMaybe 0 (x:xs) = Just x
findIndexMaybe n (x:xs) = findIndexMaybe (n-1) xs

--O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM c (M ks xs) =
  let
    i = indexK c ks 0
  in
    M (borrarEn i ks) (borrarEn i xs)

borrarEn :: Int -> [a] -> [a]
borrarEn n [] = [] --Si se quiere hacer parcial debería dar error
borrarEn 0 (x:xs) = xs
borrarEn n (x:xs) = borrarEn (n-1) xs

--O(1)
keys :: Map k v -> [k]
keys (M ks xs) = ks
