--Viejo
unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color.
nroBolitas co1 CeldaVacia = 0
nroBolitas co1 (Bolita co2 ce) = unoSi(esMismoColor co1 co2) + nroBolitas co1 ce

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False

poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner co ce = Bolita co ce

sacar :: Color -> Celda -> Celda
--dado un color y una celda, quita una bolita de dicho color de la celda.
sacar co CeldaVacia = CeldaVacia
sacar co (Bolita co2 ce) = if(esMismoColor co co2)
                            then ce else Bolita co2 (sacar co ce)

ponerN :: Int -> Color -> Celda -> Celda
--dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 co ce = ce
ponerN n co ce = Bolita co (ponerN (n-1) co ce)

----Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

unCamino = Nada (Cofre [Cacharro,Cacharro] (Nada (Cofre [Tesoro] Fin)))
unCamino2 = Fin
unCamino3 = Nada (Nada (Cofre [Tesoro, Cacharro, Tesoro, Tesoro, Tesoro] (Nada(Cofre [Tesoro] Fin))))

hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin = False
hayTesoro (Cofre xs c) = hayTesoroEntre xs || hayTesoro c
hayTesoro (Nada c) = hayTesoro c

hayTesoroEntre :: [Objeto] -> Bool
hayTesoroEntre [] = False
hayTesoroEntre (x:xs) = esTesoro x || hayTesoroEntre xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre
--precon: Tiene que haber al menos un tesoro
pasosHastaTesoro Fin = error("No cumple precondicion.")
pasosHastaTesoro (Cofre xs c) = if(hayTesoroEntre xs)
                                  then 0 else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

{-
hayTesoroEnFeo :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad de pasos.
hayTesoroEnFeo 0 (Cofre xs c) = hayTesoroEntre xs
hayTesoroEnFeo 0 _ = False
hayTesoroEnFeo n Fin = False
hayTesoroEnFeo n (Cofre xs c) = hayTesoroEnFeo (n-1) c
hayTesoroEnFeo n (Nada c) = hayTesoroEnFeo (n-1) c
-}

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n c = hayTesoroAca(avanzarNPasos n c)

hayTesoroAca :: Camino -> Bool
hayTesoroAca (Cofre xs c) = hayTesoroEntre xs
hayTesoroAca _ = False

alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros 0 _ = True
alMenosNTesoros n Fin = False
alMenosNTesoros n (Cofre xs c) = alMenosNTesoros (max 0 (n - (cantTesoros xs))) c
alMenosNTesoros n (Nada c) = alMenosNTesoros n c

cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay
--en ese rango.
cantTesorosEntre n1 n2 c = cantTesorosHasta (avanzarNPasos n1 c) (n2 - n1)

avanzarNPasos :: Int -> Camino -> Camino
avanzarNPasos 0 c = c
avanzarNPasos n Fin = Fin
avanzarNPasos n (Cofre _ c) = avanzarNPasos (n-1) c
avanzarNPasos n (Nada c) = avanzarNPasos (n-1) c

cantTesorosHasta :: Camino -> Int -> Int
--Precon: El número dado es mayor que 0.
cantTesorosHasta c 0 = cantTesorosSiHay c
cantTesorosHasta Fin n = 0
cantTesorosHasta (Cofre o c) n = cantTesoros o + cantTesorosHasta c (n-1)
cantTesorosHasta (Nada c) n =  cantTesorosHasta c (n-1)

sigCamino :: Camino -> Camino
--PARCIAL
--Precon: El camino dado no es Fin.
sigCamino (Cofre _ c) = c
sigCamino (Nada c) = c

cantTesorosSiHay :: Camino -> Int
cantTesorosSiHay (Cofre xs c) = cantTesoros xs
cantTesorosSiHay _ = 0

cantTesoros :: [Objeto] -> Int
cantTesoros [] = 0
cantTesoros (x:xs) = unoSi(esTesoro x) + cantTesoros xs

--2 Tipos arboreos
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

arbolPrueba = NodeT 1
                (NodeT 2
                  EmptyT
                  EmptyT)
                (NodeT 3
                  EmptyT
                  EmptyT)

unArbol = NodeT 5 
            (NodeT 4 
              (NodeT 3 
                (NodeT 2 
                  EmptyT 
                  (NodeT 1 
                    EmptyT 
                    EmptyT
                  )
                )
                (NodeT 2
                  (NodeT 1
                    EmptyT
                    EmptyT
                  )
                  EmptyT
                )
              )
              EmptyT
            )
            (NodeT 4 
              (NodeT 3 
                (NodeT 2 
                  EmptyT 
                  EmptyT 
                )
                (NodeT 2
                  EmptyT
                  (NodeT 1
                    EmptyT
                    EmptyT
                  )
                )
              )
              EmptyT
            )

unArbol2 = EmptyT
unArbol3 = NodeT 4 
            EmptyT 
            (NodeT 5 
              EmptyT 
              (NodeT 2 
                (NodeT 2 
                  EmptyT 
                  EmptyT
                )
                (NodeT 1
                  EmptyT
                  EmptyT
                )
              )
            )

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
--Dado un arbol binario devuelve su cantidad de elementos
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y t1 t2) = x==y || perteneceT x t1 || perteneceT x t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT = 0
aparicionesT x (NodeT y t1 t2) = unoSi(x==y) + aparicionesT x t1 + aparicionesT x t2

leaves :: Tree a -> [a]
--Dado un arbol devuelve los elementos que se encuentran en sus hojas
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x t1 t2) = leaves t1 ++ leaves t2

heightT :: Tree a -> Int
--dado un arbol devuelve su altura
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + (max (heightT t1) (heightT t2))

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = (toList t1) ++ [x] ++ (toList t2)

levelN :: Int -> Tree a -> [a]
--Dado un número n y un arbol devuelve una lista con los nodos de nivel n.
levelN 0 (NodeT x t1 t2) = [x]
levelN _ EmptyT = []
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

{-Sigue sin funcionar con la correción de clases
y ademas me falta re implementar para no sobre recorrer el arbol

listPerLevel :: Tree a -> [[a]]
--Dado un arbol devuelve una lista de listas en la que cada elemento repre
--senta un nivel de dicho arbol
listPerLevel EmptyT = []
listPerLevel t = (cadaLevel (heightT t) t)

cadaLevel :: Int -> Tree a -> [[a]]
--Precon: Hay tantos niveles como el dado.
cadaLevel 0 EmptyT = []
cadaLevel 0 t = [levelN 0 t]
cadaLevel n t = (levelN n t) : cadaLevel (n-1) t

-}

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : levelConLevel (listPerLevel t1) (listPerLevel t2)

levelConLevel :: [[a]] -> [[a]] -> [[a]]
levelConLevel [] yss = yss
levelConLevel xss [] = xss
levelConLevel (xs:xss) (ys:yss) = (xs++ys) : levelConLevel xss yss

--en realidad se le llama root
rootT :: Tree a -> a
--precon: el arbol dado no es emptyT.
rootT (NodeT x t1 t2) = x

ramaMasLargaMenosEficiente :: Tree a -> [a]
ramaMasLargaMenosEficiente EmptyT = []
ramaMasLargaMenosEficiente (NodeT x t1 t2) = x : listaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga x y = if (length x > length y) then x else y

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = 
    agregarEnTodos x ((todosLosCaminos t1) ++ (todosLosCaminos t2))

agregarEnTodos :: a -> [[a]] -> [[a]]
agregarEnTodos x [] = [[x]]
agregarEnTodos x (ys:yss) = (x:ys) : agregarEnTodos x yss

--Devuelve todos los caminos maximales.
todosLosCaminosMaximal :: Tree a -> [[a]]
todosLosCaminosMaximal EmptyT = []
todosLosCaminosMaximal (NodeT x t1 t2) =
  agregarEnCadaOSiMismo x ((todosLosCaminosMaximal t1) ++ (todosLosCaminosMaximal t2))

agregarEnCadaOSiMismo :: a -> [[a]] -> [[a]]
--Dado un elemento y una lista agrega el elemento en cada elemento de la lista.
--Si la lista es vacia agrega el elemento en una lista de listas.
agregarEnCadaOSiMismo x [] = [[x]]
--Caso borde (evita que el elemento se agregue en una lista solo):
agregarEnCadaOSiMismo x [ys] = [x:ys]
agregarEnCadaOSiMismo x (ys:yss) = (x:ys) : agregarEnCadaOSiMismo x yss

--Probando otra implmentacion
todosMaximal :: Tree a -> [[a]]
todosMaximal EmptyT = []
todosMaximal (NodeT x t1 t2) =
  let recursion = todosMaximal t1 ++ todosMaximal t2
  in
    if (null recursion)
      then [x] : recursion
      else agregarEnCada x recursion

agregarEnCada :: a -> [[a]] -> [[a]]
agregarEnCada x [] = []
agregarEnCada x (y:ys) = (x:y) : agregarEnCada x ys

--Expresiones aritmeticas

data ExpA = Valor Int
	| Sum ExpA ExpA
        | Prod ExpA ExpA
        | Neg ExpA

exp1 = Sum (Valor 0) (Prod (Neg (Neg (Valor 4))) (Valor 3))
exp2 = Neg (Valor 20)
exp3 = Prod (Neg (Valor 3)) (Valor 0)

eval :: ExpA -> Int
--Dada una expresión aritmetica devuelve el resultado de evaluarla-
eval (Valor x) = x
eval (Sum x y) = (eval x) + (eval y)
eval (Prod x y) =  (eval x) * (eval y)
eval (Neg x) = -(eval x)

simplificar :: ExpA -> ExpA
--Dada una expresión aritmetica, la simplifica según los siguientes
--criterios (descritos utilizando notación matemática conencional)
--simplificar 
simplificar (Valor x) = Valor x 
simplificar (Sum x y) = simplificarSum (simplificar x) (simplificar y)
simplificar (Prod x y) = simplificarProd (simplificar x) (simplificar y)
simplificar (Neg x) = simplificarNeg (simplificar x)

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) e = e
simplificarSum e (Valor 0) = e
simplificarSum e1 e2 = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd e1 e2 = Prod e1 e2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg x) = x
simplificarNeg x = Neg x

---Intentando mejorar ramaMasLarga
ramaMasLarga :: Tree a -> [a]
ramaMasLarga t = snd(ramaMasLargaConL t)

ramaMasLargaConL :: Tree a -> (Int, [a])
ramaMasLargaConL EmptyT = (0, [])
ramaMasLargaConL (NodeT x t1 t2) =
      let 
        (largo1,elementos1) = (ramaMasLargaConL t1)
        (largo2,elementos2) = (ramaMasLargaConL t2)
      in
        if (largo1 > largo2) 
          then (largo1+1, x : elementos1)
          else (largo2+1, x : elementos2)
