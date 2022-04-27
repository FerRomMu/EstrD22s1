module Set2
  (Set2, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

--Funciones de guias anteriores

agregarSiNoEsta :: Eq a => a -> [a] -> [a]
agregarSiNoEsta x [] = x : []
agregarSiNoEsta x (y:ys) =
	if (x == y)
	  then y:ys
	  else y : agregarSiNoEsta x ys

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en
--xs que sea igual.
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el lardo ge 
--esa lista, es decir, la cantidad de elementos que posee. 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) =
  if (x == y)
    then ys
    else y : remove x ys
-------------------------------

data Set2 a = S [a]
{--INVARIANTE DE REPRESENTACION:
  *La lista de elementos no tiene repetidos.
--}

--O(1)
emptyS :: Set2 a
emptyS = S []

--O(n)
addS :: Eq a => a -> Set2 a -> Set2 a
addS x (S xs) = S (agregarSiNoEsta x xs)

--O(n)
belongs :: Eq a => a -> Set2 a -> Bool
belongs x (S xs) = pertenece x xs

--O(n)
sizeS :: Eq a => Set2 a -> Int
sizeS (S xs) = longitud xs

--O(n)
removeS :: Eq a => a -> Set2 a -> Set2 a
removeS x (S xs) = S(remove x xs)

--O(n²)
unionS :: Eq a => Set2 a -> Set2 a -> Set2 a
unionS (S xs) s = addAllS xs s

addAllS :: Eq a => [a] -> Set2 a -> Set2 a
addAllS [] s = s
addAllS (x:xs) s = addS x (addAllS xs s)

--O(1)
setToList :: Eq a => Set2 a -> [a]
setToList (S xs) = xs
