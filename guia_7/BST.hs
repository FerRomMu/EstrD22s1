
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--De otras guias
--O(1)
root :: Tree a -> a
--precon hay root
root (NodeT x ti td) = x

--O(n) donde n es la cantidad de elementos del arbol dado.
heightT :: Tree a -> Int
--dado un arbol devuelve su altura
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + (max (heightT t1) (heightT t2))
----------------

t1 = insertBST 2(insertBST 3(insertBST 2(insertBST 0(insertBST 5(EmptyT)))))
t2 = insertBST 3(insertBST 1(insertBST 2(EmptyT)))
t3 = EmptyT

--TODAS LAS SIGUIENTES FUNCIONES TIENEN COMO PRECONDICION QUE LOS ARBOLES
--DADOS SEAN BST.

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST x EmptyT = False
belongsBST x (NodeT y ti td) =
  if(x == y)      then True 
  else if (x < y) then belongsBST x ti
                  else belongsBST x td

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) =
  if(x == y)      then NodeT x ti td
  else if (x < y) then NodeT y (insertBST x ti) td
                  else NodeT y ti (insertBST x td)

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y ti td) =
  if(x == y)      then rearmarBST ti td
  else if (x < y) then NodeT y (deleteBST x ti) td
                  else NodeT y ti (deleteBST x td)

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = td
rearmarBST ti td =
  let
    (maxTi, tiSinMAx) = splitMaxBST ti
  in
    NodeT maxTi tiSinMAx td

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
--precon: No es un arbol vacio.
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST EmptyT = error "No hay minimo."
splitMinBST (NodeT x EmptyT td) = (x, td) --Caso borde
splitMinBST (NodeT x ti td) =
  let
    (min, t) = splitMinBST ti
  in
    (min, NodeT x t td)

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
--precon: No es un arbol vacio.
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST EmptyT = error "No hay maximo."
splitMaxBST (NodeT x ti EmptyT) = (x, ti) --Caso borde
splitMaxBST (NodeT x ti td) =
  let
    (max, t) = splitMaxBST td
  in
    (max, NodeT x ti t)

--O(n²) donde n es la cantidad de elementos del arbol dado.
esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x ti td) = esMayorARoot x ti && esMenorARoot x td
                        && esBST ti && esBST td

--O(1)
esMayorARoot :: Ord a => a -> Tree a -> Bool
esMayorARoot x EmptyT = True
esMayorARoot x t = x > root t

--O(1)
esMenorARoot :: Ord a => a -> Tree a -> Bool
esMenorARoot x EmptyT = True
esMenorARoot x t = x < root t

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) =
  if (x>y)      
    then chooseMaybe y (elMaximoMenorA x td)
    else elMaximoMenorA x ti

--O(log t) donde t es la cantidad de elementos del arbol
--siempre y cuando no se desbalance.
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA x EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) =
  if (x<y)      
    then chooseMaybe y (elMinimoMayorA x ti)
    else elMinimoMayorA x td

--O(n²) donde n es cada elemento del arbol.
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x ti td) =
  abs (heightT ti - heightT td) <= 1
  && balanceado ti
  && balanceado td

chooseMaybe :: a -> Maybe a -> Maybe a
chooseMaybe x Nothing = Just x
chooseMaybe x y = y
