module Map
  (Map, emptyM, assocM, lookupM, deleteM, keys)
where

--data Maybe a = Nothing | Just a

data Map k a = M [(k, a)]

--O(1)
emptyM :: Map a k
emptyM = M []

--O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM c x (M xs) = M ((c,x):xs)

--O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM c (M xs) = findMaybe c xs

--O(n)
findMaybe :: Eq k => k -> [(k, v)] -> Maybe v
findMaybe c [] = Nothing
findMaybe c ((cl,x):xs) =
  if (c == cl)
    then Just x
    else findMaybe c xs

--O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM c (M xs) = M (borrarTuplas c xs)

--O(n)
borrarTuplas :: Eq k => k -> [(k,v)] -> [(k,v)]
borrarTuplas c [] = []
borrarTuplas c (x:xs) =
  if (c == fst x)
    then borrarTuplas c xs
    else x : borrarTuplas c xs

--O(n²)
keys :: Eq k => Map k v -> [k]
keys (M xs) = fstAll xs

--O(n²)
fstAll :: Eq k => [(k,v)] -> [k]
fstAll [] = []
fstAll (x:xs) = agregarSiNoEsta (fst x) (fstAll xs)

--O(n)
agregarSiNoEsta :: Eq a => a -> [a] -> [a]
agregarSiNoEsta x [] = x : []
agregarSiNoEsta x (y:ys) =
	if (x == y)
	  then y:ys
	  else y : agregarSiNoEsta x ys
