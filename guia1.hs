-- PARTE 1

-- Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor x = x + 1

-- Dados dos números devuelve su suma utilizando la operación +
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Dado dos números, devuelve un par donde la primera componente es la división por el segundo
-- y la segunda componennte es el resto de dicha división.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

--Dado un par de números devuelve el mayor de estos
maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = if (x > y) then x else y

--Punto escrito:
{-
De 4 ejemplos de expresiones diferentes que denoten el numero 10:
ej 1: sucesor (sumar 5 4)
ej 2: maxDelPar (5, (sucesor 9))
ej 3: sumar (sucesor 0) (maxDelPar (divisionYResto 19 10))
ej 4: sumar (sucesor 4) 5
-} 

-- PARTE 2: Tipos enumerativos

data Dir = Norte | Este | Sur | Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales Sur Sur = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--hecho en clases
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues _ _ = False

--basado en sugerencia de Fidel
vieneDespues2 :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues2 Lunes Domingo = True
vieneDespues2 d1 d2 = diaDeSemanaEnNumero d1 > diaDeSemanaEnNumero d2

diaDeSemanaEnNumero :: DiaDeSemana -> Int
diaDeSemanaEnNumero Lunes = 1
diaDeSemanaEnNumero Martes = 2
diaDeSemanaEnNumero Miercoles = 3
diaDeSemanaEnNumero Jueves = 4
diaDeSemanaEnNumero Viernes = 5
diaDeSemanaEnNumero Sabado = 6
diaDeSemanaEnNumero Domingo = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--3
negar :: Bool -> Bool
negar False = True
negar True = False

--Versiones no tan felices (previos a corrección)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or _ _ = False

--Post corrección (evitar miedo al booleano)
implica2 :: Bool -> Bool -> Bool
implica2 True b = b
implica2 _ _ = True

and2 :: Bool -> Bool -> Bool
and2 True b = b
and2 _ _ = False

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ b = b

-- PARTE 3

data Persona = P String Int

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (P _ e) = (P n e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) then p1 else p2

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = Pok TipoDePokemon Int
data Entrenador = E String Pokemon Pokemon

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

unoSi :: Bool -> Int
unoSi True = 1
unoSi _ = 0

--Poco feliz grita SUBTAREA
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = if (esTipo t p1 && esTipo t p2)
                                      then 2
                                      else if (esTipo t p1 || esTipo t p2) 
                                        then 1
                                        else 0

cantidadDePokemonDe2 :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe2 t (E _ p1 p2) = unoSi(esTipo t p1) + unoSi(esTipo t p2)

pokemonALista :: Entrenador -> [Pokemon]
pokemonALista (E _ p1 p2) = [p1,p2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonALista e1 ++ pokemonALista e2

-- PARTE 4
loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{-¿Porque estas funciones son polimorficas?
Porque pueden recibir cualquier tipo (en swap incluso hasta dos tipos distintos que serian a y b).
y funcionar de igual manera. -}

-- PARTE 5
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:xs) = x
--precon: Debe haber Primero

sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs
--Precon: Debe haber primero

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x,xs)
--Precon: Debe haber primero
