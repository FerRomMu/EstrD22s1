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

tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene solo salsa y queso
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa x p) = (esSalsa x || esQueso x) 
                                  && tieneSoloSalsaYQueso p 

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
caminoAlTesoro m = snd (caminoAlTesoroConDir m)

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
caminoDeLaRamaMasLarga m = snd(caminoMasLargoContando m)

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
