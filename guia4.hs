data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int

cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + (cantidadDeCapas p)

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamon a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa x p) = if (esJamon x)
                        then sacarJamon p
                        else Capa x (sacarJamon p)

--Viendo la duda de discord me doy cuenta que entendí mal el enunciado. Lo dejo con
--otro nombre mas adecuado y lo vuelve a resolver mas abajo.
--Lo que este evalua es que la composición de la pizza sea solo de queso y salsa, 
--pero no evalua que haya ambos.
tieneSoloSalsaYQuesoOAlgunoDeLosDos :: Pizza -> Bool
--Dice si una pizza tiene solo salsa y queso
tieneSoloSalsaYQuesoOAlgunoDeLosDos Prepizza = True
tieneSoloSalsaYQuesoOAlgunoDeLosDos (Capa x p) = (esSalsa x || esQueso x) 
                                  && tieneSoloSalsaYQuesoOAlgunoDeLosDos p 

--En este caso evalua que solo haya queso y salsa, puede haber mas queso o mas salsa pero es necesario que haya al menos uno de ambos.
--Como hace 3 recorridos sobre la pizza para corroborar los ingredientes entiendo
--que puede no ser la solucion mas feliz.
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso p = tieneQueso p && tieneSalsa p && not (tieneIngredienteEspecial p)

--En esta solucion recorro la estructura entera solo una vez guardando los ingredientes.
--Luego evaluo igual que en la solucion anterior(3 recursiones), 
--con la diferencia de que la recursion es sobre una pizza de como maximo 4 capas.
--Por lo tanto, en pizzas pequeñas es mejor la anterior solucion y en pizzas con
--muchos ingredientes esta.
tieneSoloSalsaYQueso2 :: Pizza -> Bool
tieneSoloSalsaYQueso2 p = tieneSoloSalsaYQueso(pizzaConUnoDeCadaIngredienteEn p)

pizzaConUnoDeCadaIngredienteEn :: Pizza -> Pizza
pizzaConUnoDeCadaIngredienteEn  Prepizza = Prepizza
pizzaConUnoDeCadaIngredienteEn  (Capa x p) =
  agregarIngredienteSiNoEsta x (pizzaConUnoDeCadaIngredienteEn p)

agregarIngredienteSiNoEsta :: Ingrediente -> Pizza -> Pizza
agregarIngredienteSiNoEsta x Prepizza = Capa x Prepizza
agregarIngredienteSiNoEsta x (Capa y p) =
  if (esMismoIngrediente x y)
    then Capa y p 
    else Capa y (agregarIngredienteSiNoEsta x p)

esMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
esMismoIngrediente Queso Queso = True
esMismoIngrediente Salsa Salsa = True
esMismoIngrediente Jamon Jamon = True
esMismoIngrediente (Aceitunas _) (Aceitunas _) = True
esMismoIngrediente _ _ = False

tieneQueso :: Pizza -> Bool
tieneQueso Prepizza = False
tieneQueso (Capa c cs) = esQueso c || tieneQueso cs

tieneSalsa :: Pizza -> Bool
tieneSalsa Prepizza = False
tieneSalsa (Capa c cs) = esSalsa c || tieneSalsa cs

tieneIngredienteEspecial :: Pizza -> Bool
--Devuelve si tiene jamon o aceitunas.
tieneIngredienteEspecial Prepizza = False
tieneIngredienteEspecial (Capa c cs) = esJamon c || esAceituna c || tieneIngredienteEspecial cs

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas x) = True
esAceituna _ = False

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa x p) = 
          Capa (duplicarSiHayAceitunas x) (duplicarAceitunas p)

duplicarSiHayAceitunas :: Ingrediente -> Ingrediente
duplicarSiHayAceitunas (Aceitunas x) = Aceitunas (x*2)
duplicarSiHayAceitunas x = x

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs

pizza = Capa (Aceitunas 8) (Capa Salsa (Capa Queso (Capa Jamon Prepizza)))
pizza2 = Prepizza
pizza3 = Capa Salsa (Capa Queso (Capa Salsa Prepizza))

--Mapa de tesoros

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa

map = Bifurcacion (Cofre [Chatarra]) 
        (Bifurcacion (Cofre [Chatarra, Chatarra, Chatarra])
          (Bifurcacion (Cofre []) (Fin (Cofre [])) (Fin (Cofre [])))
          (Bifurcacion (Cofre [Chatarra, Tesoro, Chatarra]) (Fin (Cofre [])) (Fin (Cofre [])))
        )
        (Fin (Cofre []))

map2 = Bifurcacion (Cofre [])
        (Bifurcacion(Cofre [])
          (Bifurcacion(Cofre [])
            (Fin (Cofre []))
            (Bifurcacion (Cofre [Chatarra, Tesoro])
              (Fin (Cofre [Chatarra]))
              (Fin (Cofre [Tesoro, Tesoro, Chatarra, Tesoro]))
            )
          )
          (Fin (Cofre [Chatarra]))
        )
        (Bifurcacion(Cofre [Chatarra, Chatarra, Chatarra])
          (Fin (Cofre[]))
          (Bifurcacion (Cofre [])
            (Fin (Cofre[]))
            (Bifurcacion (Cofre [Chatarra, Tesoro, Chatarra])
              (Bifurcacion (Cofre[])
                (Fin (Cofre[]))
                (Bifurcacion (Cofre[])
                  (Fin (Cofre[]))
                  (Fin (Cofre[]))
                )
              )
              (Fin (Cofre[]))
            )
          )
        )
          
map3 = Fin(Cofre [Chatarra, Chatarra])

--1.
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = (hayTesoroEnCofre c) 
                                || (hayTesoro m1) 
                                || (hayTesoro m2)

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEntre os

hayTesoroEntre :: [Objeto] -> Bool
hayTesoroEntre [] = False
hayTesoroEntre (o:os) = (esTesoro o) || (hayTesoroEntre os)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn d m = hayTesoroEnCofre(obtenerCofreSiHayEn d m)

obtenerCofreSiHayEn :: [Dir] -> Mapa -> Cofre
--Aclaracion: Funcion absoluta, si no hay camino suficiente en el mapa devuelve un
--cofre vacio.
obtenerCofreSiHayEn [] m = cofreEn m
--Una mejor solucion seria usando el tipo just para evitar el cofre vacio
obtenerCofreSiHayEn (x:xs) (Fin c) = Cofre [] --Caso borde, no hay camino suficiente
obtenerCofreSiHayEn (x:xs) (Bifurcacion c m1 m2) = 
                          if(esIzq x) 
                            then obtenerCofreSiHayEn xs m1 
                            else obtenerCofreSiHayEn xs m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

cofreEn :: Mapa -> Cofre
cofreEn (Fin c) = c
cofreEn (Bifurcacion c m1 m2) = c

--Mismo ejercicio pero de manera parcial
hayTesoroEnParcial :: [Dir] -> Mapa -> Bool
--precon: Hay camino en el mapa para llegar con las direcciones dadas.
hayTesoroEnParcial d m = hayTesoroEnCofre(cofreEn(avanzarHasta d m))

avanzarHasta :: [Dir] -> Mapa -> Mapa
--precon: Hay camino en el mapa para llegar con las direcciones dadas.
avanzarHasta [] m = m
avanzarHasta (x:xs) (Fin c) = error "no cumplio precondición"
avanzarHasta (x:xs) (Bifurcacion c m1 m2) =
                       if(esIzq x) 
                        then avanzarHasta xs m1
                        else avanzarHasta xs m2

--3.
caminoAlTesoro :: Mapa -> [Dir]
--precon: Existe tesoro y es unico.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) =
  if (hayTesoroEnCofre c)
    then []
    else if (hayTesoro m1)
      then Izq : caminoAlTesoro m1
      else Der : caminoAlTesoro m2

--3 alternativo
caminoAlTesoroWrapper :: Mapa -> [Dir]
--precon: Existe tesoro y es unico.
caminoAlTesoroWrapper m = snd (caminoAlTesoroConDir m)

--Entiendo que es choclodigo pero no se me ocurre otra forma
--de poder verificar si hay tesoro en esa rama para irla guardando
--de manera recursiva.
caminoAlTesoroConDir :: Mapa -> (Bool, [Dir])
caminoAlTesoroConDir (Fin c) = (hayTesoroEnCofre c, [])
caminoAlTesoroConDir (Bifurcacion c m1 m2) =
  let 
    (hayTesoroPorIzq, dirs1) = caminoAlTesoroConDir m1
    (hayTesoroPorDer, dirs2) = caminoAlTesoroConDir m2
  in
  --mira si ya alguna rama tiene el camino al tesoro
    if (hayTesoroPorIzq)
    then (hayTesoroPorIzq, Izq : dirs1)
    else if (hayTesoroPorDer)
      then (hayTesoroPorDer, Der : dirs2)
      --En caso negativo, se fija si este nodo tiene tesoro y genera la base.
      else (hayTesoroEnCofre c, [])
      
--4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) =
  let
    camino1 = caminoDeLaRamaMasLarga m1
    camino2 = caminoDeLaRamaMasLarga m2
  in
    if(length camino1 > length camino2)
      then Izq : camino1
      else Der : camino2

caminoDeLaRamaMasLargaWrapper :: Mapa -> [Dir]
caminoDeLaRamaMasLargaWrapper m = snd(caminoMasLargoContando m)

caminoMasLargoContando :: Mapa -> (Int, [Dir])
caminoMasLargoContando (Fin c) = (1, [])
caminoMasLargoContando (Bifurcacion c m1 m2) =
	let
	  (length1, dirs1) = caminoMasLargoContando m1
	  (length2, dirs2) = caminoMasLargoContando m2
	in
	  if (length1 > length2)
	    then (length1 + 1, Izq:dirs1)
	    else (length2 + 1, Der:dirs2)

--5
tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el arbol.
tesorosPorNivel (Fin c) = tesorosEnCofre c : []
tesorosPorNivel (Bifurcacion c m1 m2) =
  (tesorosEnCofre c) : nivelConNivel (tesorosPorNivel m1) (tesorosPorNivel m2)

tesorosEnCofre :: Cofre -> [Objeto]
tesorosEnCofre (Cofre o) = tesorosEn o

tesorosEn :: [Objeto] -> [Objeto]
tesorosEn [] = []
tesorosEn (o:os) = singularSi(esTesoro o) o ++ tesorosEn os

singularSi :: Bool -> a -> [a]
singularSi True x = [x]
singularSi _ _ = []

nivelConNivel :: [[a]] -> [[a]] -> [[a]]
nivelConNivel [] ys = ys
nivelConNivel xs [] = xs
nivelConNivel (x:xs) (y:ys) = (x++y) : nivelConNivel xs ys

--6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = []
todosLosCaminos (Bifurcacion c m1 m2) =
  agregarEnTodos Izq (todosLosCaminos m1) 
  ++
  agregarEnTodos Der (todosLosCaminos m2)

agregarEnTodos :: a -> [[a]] -> [[a]]
agregarEnTodos x [] = [x] : []
--caso borde para evitar que siempre se agregue una lista con el elemento d
--si hay elementos en xs corta en el último elemento, si no va al caso base.
agregarEnTodos x [y] = (x:y) : []
agregarEnTodos x (y:ys) = (x:y) : agregarEnTodos x ys

--Nave Espacial

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

arma = LanzaTorpedos
m1 = Motor 500
m2 = Motor 143
m3 = Motor 220
al1 = Almacen [Comida, Comida, Combustible, Torpedo]
al2 = Almacen [Oxigeno, Torpedo, Oxigeno]
al3 = Almacen [Combustible, Comida, Comida, Combustible, Comida]

s1 = S "Sector A" [arma, arma, m1, al2] ["Pepe","Jose","Josema"]
s2 = S "Sector B" [al1,al3] ["Neo"]
s3 = S "Sector C" [m2, arma, arma, al2] ["Neo"]
s4 = S "Sector D" [] []
s5 = S "Sector S" [m1,m2,m3,al1,al2,al3,arma] ["Capitan"]

n1 = N (EmptyT)
n2 = N (NodeT s1
          EmptyT
          (NodeT s4
            EmptyT
            EmptyT
          )
       )
n3 = N (NodeT s5
          (NodeT s4
            EmptyT
            (NodeT s3
              EmptyT
              EmptyT
            )
          )
          (NodeT s2
            EmptyT
            (NodeT s1
              EmptyT
              EmptyT
            )
          )
       )
--1
sectores :: Nave -> [SectorId]
--Devuelve todos los sectores de la nave
sectores (N ts) = losIdDeSectores ts

losIdDeSectores :: Tree Sector -> [SectorId]
losIdDeSectores EmptyT = []
losIdDeSectores (NodeT s ti td) =
  idDe s : (losIdDeSectores ti ++ losIdDeSectores td)

idDe :: Sector -> SectorId
idDe (S sid c t) = sid

--2
poderDePropulsion :: Nave -> Int
--Devuelve la suma de poder de propulsión de todos los motores de la nave.
poderDePropulsion (N ts) = poderDePropulsionEn ts

poderDePropulsionEn :: Tree Sector -> Int
poderDePropulsionEn EmptyT = 0
poderDePropulsionEn (NodeT s ti td) =
  (poderDePropulsionDeSector s) + poderDePropulsionEn ti + poderDePropulsionEn td

poderDePropulsionDeSector :: Sector -> Int
poderDePropulsionDeSector (S _ c _) = poderDePropulsionDeComponentes c

poderDePropulsionDeComponentes :: [Componente] -> Int
poderDePropulsionDeComponentes [] = 0
poderDePropulsionDeComponentes (x:xs) =
  (poderDePropulsionDeComponente x) + poderDePropulsionDeComponentes xs

poderDePropulsionDeComponente :: Componente -> Int
poderDePropulsionDeComponente (Motor x) = x
poderDePropulsionDeComponente _ = 0

--3
barriles :: Nave -> [Barril]
--Devuelve tdos los barriles de la nave
barriles (N ts) = barrilesEn ts

barrilesEn :: Tree Sector -> [Barril]
barrilesEn EmptyT = []
barrilesEn (NodeT s ti td) =
  barrilesEnSector s ++ barrilesEn ti ++ barrilesEn td

barrilesEnSector :: Sector -> [Barril]
barrilesEnSector (S _ c _) = barrilesEnComponentes c

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes [] = []
barrilesEnComponentes (x:xs) =
  barrilesEnComponente x ++ barrilesEnComponentes xs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen b) = b
barrilesEnComponente _ = []

--4
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector c id (N ts) = N (agregadoEnSector c id ts)

agregadoEnSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregadoEnSector c id EmptyT = EmptyT
agregadoEnSector c id (NodeT s ti td) =
  if (esSector id s)
    then NodeT (agregarComponentes c s) ti td
    else NodeT s (agregadoEnSector c id ti) (agregadoEnSector c id td)

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes c (S id sc t) = S id (c++sc) t

esSector :: SectorId -> Sector -> Bool
esSector id (S sid _ _) = id == sid

--5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Incorpora un tripulante a una lista de sectores de la nave.
asignarTripulanteA t id (N ts) = N (asignarEn t id ts)

asignarEn :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarEn t ids EmptyT = EmptyT
asignarEn t ids (NodeT s ti td) =
  if (haySector ids s)
    then NodeT (asignarAca t s) (asignarEn t ids ti) (asignarEn t ids td)
    else NodeT s (asignarEn t ids ti) (asignarEn t ids td)

haySector :: [SectorId] -> Sector -> Bool
haySector [] s = False
haySector (x:xs) s = esSector x s || haySector xs s

asignarAca :: Tripulante -> Sector -> Sector
asignarAca t (S id c st) = S id c (t:st)

--6
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresDondeEsta t ts

sectoresDondeEsta :: Tripulante -> Tree Sector -> [SectorId]
sectoresDondeEsta t EmptyT = []
sectoresDondeEsta t (NodeT s ti td) =
  singularSi(esTripulanteEn t s) (idDe s) ++ sectoresDondeEsta t ti ++ sectoresDondeEsta t td

esTripulanteEn :: Tripulante -> Sector -> Bool
esTripulanteEn t (S _ _ ts) = elem t ts

--7
tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = tripulantesEnSinRepetir ts

tripulantesEnSinRepetir :: Tree Sector -> [Tripulante]
tripulantesEnSinRepetir EmptyT = []
tripulantesEnSinRepetir (NodeT s t1 t2) =
  unirSinRepetidos (tripulantesEnSector s) 
	(unirSinRepetidos (tripulantesEnSinRepetir t1) (tripulantesEnSinRepetir t2))

agregarSiNoEsta :: Eq a => a -> [a] -> [a]
agregarSiNoEsta x [] = x : []
agregarSiNoEsta x (y:ys) =
	if (x == y)
	  then y:ys
	  else y : agregarSiNoEsta x ys

unirSinRepetidos :: Eq a => [a] -> [a] -> [a]
unirSinRepetidos [] ys = ys
unirSinRepetidos (x:xs) ys = agregarSiNoEsta x (unirSinRepetidos xs ys)

tripulantesFeo :: Nave -> [Tripulante]
tripulantesFeo (N ts) = sinRepetidos(tripulantesEn ts)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
  if(elem x xs)
    then sinRepetidos xs
    else x : sinRepetidos xs

tripulantesEn :: Tree Sector -> [Tripulante]
tripulantesEn EmptyT = []
tripulantesEn (NodeT s t1 t2) =
  (tripulantesEnSector s) ++ (tripulantesEn t1) ++ (tripulantesEn t2)

tripulantesEnSector :: Sector -> [Tripulante]
tripulantesEnSector (S _ _ t) = t

--Manada de lobos

type Presa = String --nombre de presa
type Territorio = String --nombre de territorio
type Nombre = String --nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre
data Manada = M Lobo

--1
ma = M (Cazador "Alfa" ["Conejo","Conejo"]
        (Explorador "Beta" ["Norte"]
          (Cria "hija")
          (Cazador "pepe" []
            (Cria "a")
            (Cria "b")
            (Cria "c")
          )
        )
        (Cria "algo")
        (Cria "bebe")
      )

ma2 = M (Cazador "Alfa" ["Conejo","Conejo", "Ciervo", "Ardilla", "Ardilla"]
        (Explorador "Beta" ["Norte"]
          (Cria "hija")
          (Cria "otra")
        )
        (Cazador "mengano" []
          (Cazador "pepe" []
            (Cria "a")
            (Cria "b")
            (Cria "c")
          )
          (Cria "d")
          (Cria "e")
        )
        (Explorador "zeta" ["Norte","Sur"]
          (Explorador "otro" ["Sur"]
            (Cria "a")
            (Cria "b")
          )
          (Cria "c")
        )
      )

ma3 = M (Explorador "wolfy" ["Norte","Sur","Este","Oeste"]
          (Cria "una")
          (Explorador "otro" ["Sur"]
            (Cria "a")
            (Cria "b")
          )
        )
--2
buenaCaza :: Manada -> Bool
--dada una manada, indica si la cant de alimento cazado es mayor a la cant
--de cris
buenaCaza (M ls) = cantidadCazada ls > cantidadCrias ls

cantidadCazada :: Lobo -> Int
cantidadCazada (Cria _) = 0
cantidadCazada (Explorador _ _ l1 l2) =
  cantidadCazada l1 + cantidadCazada l2
cantidadCazada (Cazador _ p l1 l2 l3) =
  length p + cantidadCazada l1 + cantidadCazada l2 + cantidadCazada l3

cantidadCrias :: Lobo -> Int
cantidadCrias (Cria _) = 1
cantidadCrias (Explorador _ _ l1 l2) =
  cantidadCrias l1 + cantidadCrias l2
cantidadCrias (Cazador _ _ l1 l2 l3) =
  cantidadCrias l1 + cantidadCrias l2 + cantidadCrias l3

--3
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M ls) = alfaEntre ls

alfaEntre :: Lobo -> (Nombre, Int)
alfaEntre (Cria n) = (n, 0)
alfaEntre (Explorador n _ l1 l2) =
    elQueMasCazo (n, 0)(elQueMasCazo(alfaEntre l1)(alfaEntre l2))
alfaEntre (Cazador n c l1 l2 l3) =
    elQueMasCazo (n, length c)
                 (elQueMasCazo (alfaEntre l1)
                               (elQueMasCazo (alfaEntre l2) (alfaEntre l3))
                 )

elQueMasCazo :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elQueMasCazo (n,i) (n2,i2) =
  if (i2 > i) then (n2, i2) else (n, i)

--4
losQueExploraron :: Territorio -> Manada -> [Nombre]
--Dado un territorio y una manada, devuelve los nombres de los exploradores
--que pasaron por dicho territorio
losQueExploraron t (M ls) = lobosQueExploraron t ls

lobosQueExploraron :: Territorio -> Lobo -> [Nombre]
lobosQueExploraron t (Cria n) = []
lobosQueExploraron t (Explorador n tl l1 l2) =
  singularSi(elem t tl) n ++
  lobosQueExploraron t l1 ++
  lobosQueExploraron t l2
lobosQueExploraron t (Cazador _ _ l1 l2 l3) =
  lobosQueExploraron t l1 ++
  lobosQueExploraron t l2 ++
  lobosQueExploraron t l3

--5
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M ls) =
  exploradoresPorTerritorioDe ls

exploradoresPorTerritorioDe :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDe (Cria n) = []
exploradoresPorTerritorioDe (Explorador n t l1 l2) =
  agregarLoboEnTerritorios n t
                         (unirTuplasSinRepetidos (exploradoresPorTerritorioDe l1)
                                                 (exploradoresPorTerritorioDe l2))
exploradoresPorTerritorioDe (Cazador _ _ l1 l2 l3) =
  unirTuplasSinRepetidos (exploradoresPorTerritorioDe l1)
                        (unirTuplasSinRepetidos 
                              (exploradoresPorTerritorioDe l2)
                              (exploradoresPorTerritorioDe l3))

agregarLoboEnTerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLoboEnTerritorios n [] ys = ys
agregarLoboEnTerritorios n (x:xs) ys = agregarLoboA n x 
                                          (agregarLoboEnTerritorios n xs ys)

agregarLoboA :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLoboA n t [] = [(t, [n])]
agregarLoboA n t (x:xs) =
  let
    (territorio, lobos) = x
  in
    if (territorio == t)
      then (territorio, agregarSiNoEsta n lobos) : xs
      else x : agregarLoboA n t xs

unirTuplasSinRepetidos :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
unirTuplasSinRepetidos [] ys = ys
unirTuplasSinRepetidos (x:xs) ys = agregarOUnirTerritorio x ys

agregarOUnirTerritorio :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarOUnirTerritorio x [] = [x]
agregarOUnirTerritorio x (y:ys) =
  let
    (t, lobos1) = x
    (territorio2, lobos2) = y
  in
    if(t == territorio2)
      then (t, unirSinRepetidos lobos1 lobos2) : ys
      else y : agregarOUnirTerritorio x ys

--opcion fea
exploradoresPorTerritorioFeo :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorioFeo (M ls) =
  cadaExploradorDe (sinRepetidos(todosLosTerritorios ls)) ls

todosLosTerritorios :: Lobo -> [Territorio]
todosLosTerritorios (Cria n) = []
todosLosTerritorios (Explorador _ t l1 l2) =
  t ++ (todosLosTerritorios l1) ++ (todosLosTerritorios l2)
todosLosTerritorios (Cazador _ _ l1 l2 l3) =
  todosLosTerritorios l1 ++
  todosLosTerritorios l2 ++
  todosLosTerritorios l3

cadaExploradorDe :: [Territorio] -> Lobo -> [(Territorio, [Nombre])]
cadaExploradorDe [] l = []
cadaExploradorDe (x:xs) l = (x,lobosQueExploraron x l) : cadaExploradorDe xs l

--6
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M ls) = fst(losSuperioresA n ls)

losSuperioresA :: Nombre -> Lobo -> ([Nombre], Bool)
losSuperioresA n (Cria nl) = ([], False)
losSuperioresA n (Explorador nl _ l1 l2) =
  let
    (n1, esSuperior1) = losSuperioresA n l1
    (n2, esSuperior2) = losSuperioresA n l2
  in
    if(esSuperior1)
      then (n1, esSuperior1)
      else (n2, esSuperior2)
losSuperioresA n (Cazador nl _ l1 l2 l3) =
  let
    (n1, esSuperior1) = losSuperioresA n l1
    (n2, esSuperior2) = losSuperioresA n l2
    (n3, esSuperior3) = losSuperioresA n l3
  in
    if(esSuperior1)
      then (nl:n1, esSuperior1)
      else if (esSuperior2)
        then (nl:n2, esSuperior2)
        else if (esSuperior3)
          then (nl:n3, esSuperior3)
          else ([], n==nl)

--Tratando de resolverlo sin usar funcion cascara. Se realiza mas recorridos sobre la
--estructura (Una por losSuperioresA2 y luego en cada una donde hay cazador para ver si es
--superior en estaLobo)
superioresDelCazadorAlternativo :: Nombre -> Manada -> [Nombre]
superioresDelCazadorAlternativo n (M l) = losSuperioresA2 n l

losSuperioresA2 :: Nombre -> Lobo -> [Nombre]
losSuperioresA2 n (Cria _) = []
losSuperioresA2 n (Explorador _ _ l1 l2) =
  unirSinRepetidos (losSuperioresA2 n l1) (losSuperioresA2 n l2)
losSuperioresA2 n (Cazador nl p l1 l2 l3) =
  let
    cazador = (Cazador nl p l1 l2 l3)
  in
    singularSi(soySuperiorA n cazador) nl ++ (unirSinRepetidos (losSuperioresA2 n l1)
    (unirSinRepetidos (losSuperioresA2 n l2) (losSuperioresA2 n l3)))

soySuperiorA :: Nombre -> Lobo -> Bool
--precon: el lobo dado es cazador.
soySuperiorA n (Cazador _ _ l1 l2 l3) = estaLobo n l1 || estaLobo n l2 || estaLobo n l3 
soySuperiorA _ _ = False

estaLobo :: Nombre -> Lobo -> Bool
estaLobo n (Cria nl) = n == nl
estaLobo n (Explorador nl _ l1 l2) =
  (n == nl) || estaLobo n l1 || estaLobo n l2
estaLobo n (Cazador nl _ l1 l2 l3) =
  (n == nl) || estaLobo n l1 || estaLobo n l2 || estaLobo n l3
