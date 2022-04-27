module Set
  (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

--Funciones de guias anteriores

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en
--xs que sea igual.
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

removeAll :: Eq a => a -> [a] -> [a]
removeAll x [] = []
removeAll x (y:ys) =
  if (x == y)
    then removeAll x ys
    else y : (removeAll x ys)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
  if (pertenece x (sinRepetidos xs))
    then sinRepetidos xs
    else x : sinRepetidos xs
-------------------------------

data Set a = S [a]
{--INVARIANTE DE REPRESENTACION:
--}

--O(1)
emptyS :: Set a
emptyS = S []

--O(1)
addS :: Eq a => a -> Set a -> Set a
addS x (S xs) = S (x:xs)

--O(n)
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = pertenece x xs

--O(n²)
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = contarSinRepetir xs []

contarSinRepetir :: Eq a => [a] -> [a] -> Int
contarSinRepetir [] ys = 0
contarSinRepetir (x:xs) ys =
  if(not (pertenece x ys))
    then 1 + contarSinRepetir xs (x:ys)
    else contarSinRepetir xs ys

--O(n)
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S(removeAll x xs)

--O(n)
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (xs++ys)

--O(n²)
setToList :: Eq a => Set a -> [a]
setToList (S xs) = sinRepetidos xs
