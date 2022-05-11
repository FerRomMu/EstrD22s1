import Empleado
import Map
import Set

module Empresa
  (consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL,
  todosLosSectores, agregarEmpleado, agregarASector, borrarEmpleado)
where

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{--
INVARIANTE DE REPRESENTACION:
  *Los id de sectores y los legajos no pueden repetirse.
  *Los empleados de los set de sectores deben estar en el map de cuils.
--}

--O(1)
consEmpresa :: Empresa
consEmpresa = ConsE EmptyMap EmptyMap

--O(log m) <- donde log m es el costo de lookupM.
--Precon: Debe haber tal empleado con dicho CUIL.
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE m mc) = fromJust(lookupM c mc)

--O(s o log m) <- donde s es el costo de setToList y log m el costo de lookupM.
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--ACLARACION: Aca la funcion es total, si se desea que falle de no existir tal sector
--se debe cambiar fromMaybe por un fromJust.
empleadosDelSector sid (ConsE ms m) = setToList (fromMaybe EmptyS (lookupM sid ms))

--O(¿1?) el costo es igual a costo de la implementacion de keys en el map.
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE m mc) = keys mc

--O(k) donde k es igual al costo de keys.
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms mc) = keys ms

--O(n*log x o s*log e) donde n*log x es el costo de agregarASectores y 
--s*log e es el costo de agregarTodosLosSectores.
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado s c (ConsE ms mc) =
  let empleado = agregarTodosLosSectores s (consEmpleado c)
  in
    ConsE (agregarASectores s empleado ms)
         (assocM c empleado mc)

--O(s*log e) donde s es la cantidad de sectores a agregar y log e el costo
--de hacer agregarSector.
agregarTodosLosSectores :: [SectorId] -> Empleado -> Empleado
agregarTodosLosSectores [] e = e
agregarTodosLosSectores (x:xs) e = agregarSector x (agregarTodosLosSectores xs e)

--O(n*log m o n*log s)) donde log m es el costo de hacer lookupM y assocM sobre ms y
--              log s el costo de hacer addS sobre el set de empleados devuelto.
--              finalmente n es la cantidad de elementos de la lista de sectores dados.
agregarASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarASectores [] e ms = ms
agregarASectores (x:xs) e ms =
  let
    sector = fromMaybe emptyS (lookupM x ms)
  in
    assocM x (addS e sector) (agregarASectores xs e ms)

--O(log m o log mc o log s o log e) 
--      donde   log m es el costo de hacer lookupM y assocM sobre ms
--              log mc es el costo de hacer lookup y assocM sobre mc
--              log s el costo de hacer addS sobre el set de empleados devuelto.
--              log e es el costo de hacer agregarSector sobre el empleado.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector sid c (ConsE ms mc) =
  let
    e = agregarSector sid (fromJust (lookupM c mc))
    sector = fromMaybe emptyS (lookupM sid ms)
    --Entiendo que si el sector no esta, se agrega con el empleado.
  in
    ConsE (assocM sid (addS e sector) ms) (assocM c e mc)

--(n*log x) donde n*log x es el costo de sectoresSinEmpleado.
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms mc) =
  let
    empleado = fromJust (lookupM c mc)
  in
    ConsE (sectoresSinEmpleado ms empleado) (deleteM c mc)

--(n*log x) donde n*log x es el costo de quitarEmpleadoDe.
sectoresSinEmpleado :: Map SectorId (Set Empleado) -> Empleado -> Map SectorId (Set Empleado)
sectoresSinEmpleado ms e = quitarEmpleadoDe ms (sectores e) e

--(n*log s o n*log m) <-- donde n es la cantidad de ids de sectores dados.
-- log s es el costo de removeS y log m es el costo de assocM y lookupM.
quitarEmpleadoDe :: Map SectorId (Set Empleado) -> [SectorId] -> Empleado -> Map SectorId (Set Empleado)
quitarEmpleadoDe ms [] e = ms
quitarEmpleadoDe ms (x:xs) e =
  let
    sector = fromJust (lookupM x ms)
  in
    assocM x (removeS sector e) (quitarEmpleadoDe ms xs e)

