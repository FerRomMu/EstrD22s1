
{-
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

Sector <- tiene componentes y tripulantes asignados.

O(1)
crearS :: SectorId -> Sector
O(1)
sectorId :: Sector -> SectorId
O(1)
componentesS :: Sector -> [Componente]
O(1)
tripulantesS :: Sector -> Set Nombre
O(1)
agregarC :: Componente -> Sector -> Sector 
O(log T )
agregarT :: Nombre -> Sector -> Sector

Tripulante <- nombre, rango y sector asignado.
Tripulante, siendo S la cantidad de sectores:
O(1)
crearT :: Nombre -> Rango -> Tripulante
O(log S)
asignarS :: SectorId -> Tripulante -> Tripulante
O(1)
sectoresT :: Tripulante -> Set SectorId
O(1)
nombre :: Tripulante -> String
O(1)
rango :: Tripulante -> Rango
-}

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{--INVARIANTES DE REPRESENTACION:
    *Los sectoresId del map de sectores se corresponden al sector del cual son key.
    *Los nombres del map de tripulantes se corresponden al tripulante del cual son key.
    *Tanto el maxheap como el map de tripulantes, deben tener todos los tripulantes de
    la nave.
    *El maxheap debe estar ordenado por el rango de tripulantes.
    *Los sectores que conoce un tripulante deben estar en el map de sectores.
    *Los tripulantes que conoce un sector deben estar en el map de tripulantes.
--}

{-COSTOS:
  Se utilizarán las siguientes letras para hacer referencia a "la cantidad de".
    * S: Donde S es la cantidad de Sectores en la nave.
    * T: Donde T es la cantidad de Tripulantes en la nave.
    * M: Donde M es la cantidad
    * C: Donde C es la cantidad
-}

--O(I*log I) donde I es la cantidad de sectoresId dados.
--JUSTIFICACION: tanto EmptyM como EmptyH son costo 1 pero construirSectores es
--costo I*log I.
construir :: [SectorId] -> Nave
construir sid = N (construirSectores sid) EmptyM EmptyH

--O(I*log I) donde I es la cantidad de sectoresId dados.
--JUSTIFICACION: assocM es costo log k (En este caso k es I) y debo
--hacer un assocM (log I) por cada sectorId dado (Costo I).
construirSectores :: [SectorId] -> Map SectorId Sector
construirSectores [] = EmptyM
construirSectores (s:ss) = 
  let
    sector = crearS s
  in
    assocM sector (construirSectores ss)

--O(log T)
--JUSTIFICACION: crearT es costo 1, pero tanto assocM en el map
--de tripulantes como insertH en el heap de tripulantes son costo
--log T.
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt ht) =
  let
    tripulante = crearT n r
  in
    N ms (assocM tripulante mt) (insertH tripulante ht)

--O(log T)
--JUSTIFICACION: La funcion lookupM en el map de tripulantes tiene costo log T
--como la funcion de sectoresT es costo 1, el costo total es log T.
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N ms mt ht) =
  case (lookupM n mt) of
    Nothing -> error "No hay tripulante dado."
    Just t -> sectoresT t

--O(log S)
--JUSTIFICACION: La funcion lookupM en el map de Sectores tiene costo log S
--como tanto tripulantesS como componentesS son de orden 1, el costo final es
--log S.
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sid (N ms mt ht) =
  case (lookupM sid ms) of
    Nothing -> error "No hay sector dado."
    Just s -> (tripulantesS s, componentesS s)

--O(T*log T)
--JUSTIFICACION: la funcion listPorRango es costo T*log T
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N ms mt ht) = toListPorRango ht

--O(T*log T)
--JUSTIFICACION: Por cada tripulante en el maxHeap tengo que
--hacer una operacion de costo 1 pero tambien una de costo log T
--(deleteMaxH), por lo tanto por cada T tengo cost log T.
toListPorRango :: MaxHeap Tripulante -> [Tripulante]
toListPorRango h =
  if(isEmptyH)
    then []
    else maxH h : deleteMaxH h
