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

--De la guia1
-------------
data Persona = P String Int

edad :: Persona -> Int
edad (P _ e) = e
-------------
--Para ejecutar
pepe = P "Jose" 38
miguel = P "Miguel" 20
ana = P "Ana" 40
------------

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if (esMayorA p n)
			then p : mayoresA n ps
			else mayoresA n ps

esMayorA :: Persona -> Int -> Bool
esMayorA p n = (edad p) > n

--Precon: La lista al menos posee una persona
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumarEdades ps) (length ps)

sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (p:ps) = (edad p) + sumarEdades ps

--Precon: Hay al menos una persona
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error("No hay personas")
elMasViejo (p:ps) = if (null ps || esMasViejoQue p (elMasViejo ps))
		then p
		else elMasViejo ps

esMasViejoQue :: Persona -> Persona -> Bool
esMasViejoQue (P _ e1) (P _ e2) = e1 > e2

--De la guia1
-------------
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = Pok TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

elementoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
elementoSuperaA Agua Fuego = True
elementoSuperaA Fuego Planta = True
elementoSuperaA Planta Agua = True
elementoSuperaA _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok t1 _) (Pok t2 _) = elementoSuperaA t1 t2

esTipo :: TipoDePokemon -> Pokemon -> Bool
esTipo tipo (Pok t _) = mismoTipo tipo t

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Fuego Fuego = True
mismoTipo Agua Agua = True
mismoTipo Planta Planta = True
mismoTipo _ _ = False
-------------
--Para ejecutar
charmander = Pok Fuego 200
squirtle = Pok Agua 180
bulbasur = Pok Planta 200
charizard = Pok Fuego 1000

ash = ConsEntrenador "Ash ketchup" [charizard, squirtle, bulbasur]
mistic = ConsEntrenador "mistic" [squirtle, squirtle]
-------------
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ poks) = length poks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonDe' t ps

cantPokemonDe' :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDe' t [] = 0
cantPokemonDe' t (pok:poks) = if (esTipo t pok)
				then 1 + cantPokemonDe' t poks
				else cantPokemonDe' t poks

losQueGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueGanan t (ConsEntrenador _ poks1) (ConsEntrenador _ poks2) = cantidadQueLesGanan (losDelTipo t poks1) poks2

losDelTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDelTipo t [] = []
losDelTipo t (pok:poks) = if (esTipo t pok)
				then pok : losDelTipo t poks
				else losDelTipo t poks

cantidadQueLesGanan :: [Pokemon] -> [Pokemon] -> Int
cantidadQueLesGanan [] poks2 = 0
cantidadQueLesGanan (pok:poks1) poks2 = if (leGanaATodos pok poks2)
					then 1 + cantidadQueLesGanan poks1 poks2
					else cantidadQueLesGanan poks1 poks2

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p [] = True
leGanaATodos p (pok:poks) = (superaA p pok) && leGanaATodos p poks

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ poks) = hayUnPokemonDeCadaTipo poks

hayUnPokemonDeCadaTipo :: [Pokemon] -> Bool
hayUnPokemonDeCadaTipo [] = False
hayUnPokemonDeCadaTipo pok = hayPokemonDe Fuego pok && hayPokemonDe Agua pok && hayPokemonDe Planta pok

hayPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDe t [] = False
hayPokemonDe t (pok:poks) =  esTipo t pok || hayPokemonDe t poks

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Managment Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

-------------
--Para ejecutar
resolverBug = ConsProyecto "Bug 202"
nuevaInterfaz = ConsProyecto "Interfaz nueva"
baseDeDato = ConsProyecto "Solucionar backend"
test = ConsProyecto "Test nueva funcionalidad"

empleadoA = Developer Junior nuevaInterfaz
empleadoB = Managment Senior nuevaInterfaz
empleadoC = Developer SemiSenior baseDeDato
empleadoD = Developer Senior test
empleadoE = Developer Senior resolverBug

unaEmpresa = ConsEmpresa [empleadoA, empleadoD, empleadoC, empleadoB, empleadoE]
-------------

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinProyectosRepetidos (proyectos' rs)

proyectos' :: [Rol] -> [Proyecto]
proyectos' [] = []
proyectos' (r:rs) = (proyectoDe r) : proyectos' rs

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Managment _ p) = p

sinProyectosRepetidos :: [Proyecto] -> [Proyecto]
sinProyectosRepetidos [] = []
sinProyectosRepetidos (p:ps) = if (estaProyecto p (sinProyectosRepetidos ps))
				then sinProyectosRepetidos ps
				else p : sinProyectosRepetidos ps

estaProyecto :: Proyecto -> [Proyecto] -> Bool
estaProyecto pr [] = False
estaProyecto pr (p:ps) = (esMismoProyecto pr p) || (estaProyecto pr ps)

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto (ConsProyecto p1) (ConsProyecto p2) = p1 == p2

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) pr = losDevSenior' rs pr

losDevSenior' :: [Rol] -> [Proyecto] -> Int
losDevSenior' [] pr = 0
losDevSenior' (r:rs) pr = if (esDevSenior r && estaEnAlgunProyecto r pr)
				then 1 + losDevSenior' rs pr
				else losDevSenior' rs pr

esDevSenior :: Rol -> Bool
esDevSenior (Developer s _) = esSenior s
esDevSenior _ = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False

estaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
estaEnAlgunProyecto r [] = False
estaEnAlgunProyecto r (p:ps) = esMismoProyecto (proyectoDe r) p || estaEnAlgunProyecto r ps

cantQueTrabajaEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajaEn pr (ConsEmpresa rs) = cantQueTrabajaEn' pr rs

cantQueTrabajaEn' :: [Proyecto] -> [Rol] -> Int
cantQueTrabajaEn' pr [] = 0
cantQueTrabajaEn' pr (r:rs) = if (estaEnAlgunProyecto r pr)
				then 1 + cantQueTrabajaEn' pr rs
				else cantQueTrabajaEn' pr rs

{-
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyecto' rs

asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' []     = []
asignadosPorProyecto' (r:rs) = if (estaProyectoEnTupla (proyectoDe r) (asignadosPorProyecto' rs))
				then sumarUnoATupla (proyectoDe r) (asignadosPorProyecto' rs)
				else (proyectoDe r, 1) : asignadosPorProyecto' rs

estaProyectoEnTupla :: Proyecto -> [(Proyecto, Int)] -> Bool
estaProyectoEnTupla pr [] = False
estaProyectoEnTupla pr (t:ts) = esMismoProyecto pr (fst t) || estaProyectoEnTupla pr ts

sumarUnoATupla :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
--precon: El proyecto dado debe estar en alguna de las tuplas.
sumarUnoATupla pr [] = []
sumarUnoATupla pr (t:ts) = if (esMismoProyecto pr (fst t))
				then (sumarUnoA t) : sumarUnoATupla pr ts
				else t : sumarUnoATupla pr ts

sumarUnoA :: (Proyecto, Int) -> (Proyecto, Int)
sumarUnoA (p,n) = (p, n+1)
-}

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyecto' rs

asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto' [] = []
asignadosPorProyecto' (r:rs) = agregarOSumarTupla r (asignadosPorProyecto' rs)

agregarOSumarTupla :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarOSumarTupla r [] = [(proyectoDe r,1)]
agregarOSumarTupla r (t:ts) = if (esMismoProyecto (proyectoDe r) (fst t))
                                then (sumarUnoA t) : ts
                                else t : agregarOSumarTupla r ts

sumarUnoA :: (Proyecto, Int) -> (Proyecto, Int)
sumarUnoA (p,n) = (p, n+1)
