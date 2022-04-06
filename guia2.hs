--PARTE !: Recursión sobre listas

sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el lardo ge 
--esa lista, es decir, la cantidad de elementos que posee. 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de 
--cada entero.
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

conjuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos
--son True.
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos
--es True.
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en
--xs que sea igual.
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e
--en xs
apariciones e [] = 0
apariciones e (x:xs) = if e == x
                        then 1 + apariciones e xs
                        else apariciones e xs

losMenoresA :: Int ->  [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if x < n
                        then x : losMenoresA n xs
                        else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de lista, devuelve la lista de aquellas
--listas que tienen mas de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = if (length xs) > n
                                  then xs : lasDeLongitudMayorA n xss
                                  else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado
--al final de la lista.
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

concatenar :: [a] -> [a] -> [a]
--Dadas dos listas devuelve la lista con todos los elementos de la primera
--lista y todos los elementos de la segunda a continuación.
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para
--adelante.
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = if x > y
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
--dada una lista devuelve el minimo
--precon: Debe haber elemento en la lista dada
elMinimo (x:xs) = elMinimoEntreYElementos x xs

elMinimoEntreYElementos :: Ord a => a -> [a] -> a
elMinimoEntreYElementos x [] = x
elMinimoEntreYElementos x (y:ys) = if x < y
                            then elMinimoEntreYElementos x ys
                            else elMinimoEntreYElementos y ys

--2.Recursión sobre números

factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número
--y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1.
--precon: n no debe ser negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos son los números
--comprendidos entre n y 1. Si el número es inferior a 1, devuelve la lista
--vacía.
cuentaRegresiva n = if n < 1
                    --caso base
                    then []
                    --caso recursivo
                    else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
--dado un número n y un elemento e devuelve una lista en la que el elemento
--e se repite n veces.
--precon: n no es negativo.
repetir 0 e = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los n primeros
--elementos de xs. Si la lista es vacía, devuelve una lista vacía.
--precon: n no es negativo.
losPrimeros 0 xs = []
losPrimeros n [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
--dados un número n y una lista xs, devuelve una lista sin los primeros n
--elementos de lista recibida. Si n es cero, deuvelve la lista completa.
--precon: n no es negativo.
sinLosPrimeros 0 xs = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--3. Registros


