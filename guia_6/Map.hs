module Map
  (Map, emptyM, assocM, lookupM, deleteM, keys)
where

--data Maybe a = Nothing | Just a

data Map k a = M [(k, a)]
--INVARIANTE DE REPRESENTACION:
--Solo puede existir una tupla con un mismo k en la lista.

--O(1)
emptyM :: Map a k
emptyM = M []

--O(n)<-- con n igual al largo de la lista xs
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM c x (M xs) = M (agregarALaTuplaElValor c x xs)

--O(n)<-- con n igual al largo de la lista dada
agregarALaTuplaElValor :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
agregarALaTuplaElValor c x [] = [(c,x)]
agregarALaTuplaElValor c x (y:ys) =
  if (c == fst y)
    then (c,x):ys
    else y : agregarALaTuplaElValor c x ys

--O(n)<-- con n igual al largo de la lista xs
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM c (M xs) = findMaybe c xs

--O(n)<-- con n igual al largo de la lista dada
findMaybe :: Eq k => k -> [(k, v)] -> Maybe v
findMaybe c [] = Nothing
findMaybe c ((cl,x):xs) =
  if (c == cl)
    then Just x
    else findMaybe c xs

--O(n)<-- con n igual al largo de la lista xs
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM c (M xs) = M (borrarTupla c xs)

--O(n)<-- con n igual al largo de la lista dada
borrarTupla :: Eq k => k -> [(k,v)] -> [(k,v)]
borrarTupla c [] = []
borrarTupla c (x:xs) =
  if (c == fst x)
    then xs
    else x : borrarTupla c xs

--O(n)<-- con n igual al largo de la lista xs
keys :: Map k v -> [k]
keys (M xs) = fstAll xs

--O(n)<-- con n igual al largo de la lista dada
fstAll :: [(a,b)] -> [a]
fstAll [] = []
fstAll (x:xs) = fst x : fstAll xs
