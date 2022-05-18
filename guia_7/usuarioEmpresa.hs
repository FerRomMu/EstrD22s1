import Empresa

--O(n*m*log x) donde es el coste de borrarNVeces.
recorteDePersonal :: Empresa -> Empresa
recorteDePersonal e =
  let
    cuils = todosLosCUIL e
  in
    borrarNVeces (div (length cuils) 2) cuils e

--O(n*m*log x) donde m*log de x es el costo de borrar empleado y n el Int dado.
borrarNVeces :: Int -> [CUIL] -> Empresa
--precon: hay n CUILs.
borrarNVeces 0 xs e = e
borrarNVeces n (x:xs) e = 
  borrarEmpleado x (borrarNVeces (n-1) xs e)

--O(n*log x) donde n*log x es el costo de agregarEmpleado.
convertirEnComodin :: CUIL -> Empresa -> Empresa
--precon: Existe un empleado en la empresa con el CUIL dado.
convertirEnComodin c e =
  let
    ids = todosLosSectores e
  in
    agregarEmpleado ids c e

eimport Empresa

--O(n*m*log x) donde es el costo de hacer agregarTodosEllos
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon sids cs = agregarTodosEllos cs (ConsEmpresa sids)

--O(n*m*log x) donde n es la cantidad de cuils y m*log x el costo
--de agregarEmpleado.
agregarTodosEllos :: [CUIL] -> Empresa -> Empresa
agregarTodosEllos [] e = e
agregarTodosEllos (c:cs) e =
  agregarEmpleado [] (consEmpleado c) (agregarTodosEllos cs e)

--O(n*m*log x) donde es el coste de borrarNVeces.
recorteDePersonal :: Empresa -> Empresa
recorteDePersonal e =
  let
    cuils = todosLosCUIL e
  in
    borrarNVeces (div (length cuils) 2) cuils e

--O(n*m*log x) donde m*log de x es el costo de borrar empleado y n el Int dado.
borrarNVeces :: Int -> [CUIL] -> Empresa
--precon: hay n CUILs.
borrarNVeces 0 xs e = e
borrarNVeces n (x:xs) e = 
  borrarEmpleado x (borrarNVeces (n-1) xs e)

--O(n*log x) donde n*log x es el costo de agregarEmpleado.
convertirEnComodin :: CUIL -> Empresa -> Empresa
--precon: Existe un empleado en la empresa con el CUIL dado.
convertirEnComodin c e =
  let
    ids = todosLosSectores e
  in
    agregarEmpleado ids c e

--Costo: O(s^2), con s la cantidad de sectores de la empresa
--Justif:  todosLosSectores es O(s), sectores es O(1)
--         y elprimero... es O(n*m) con 
--          n=s (total de sectores de la empresa) y
--          m=s (Máximo de sectores del empleado)
esComodin :: CUIL -> Empresa -> Bool
esComodin c e = 
  let
    empleado = buscarPorCuil c e
  in
    elPrimeroEstaContenidoEn (todosLosSectores e) (sectores empleado)

--O(n*m) donde n es la primer lista y m la segunda
elPrimeroEstaContenidoEn Eq a => [a] -> [a] -> Bool
elPrimeroEstaContenidoEn [] ys = True
elPrimeroEstaContenidoEn (x:xs) ys = 
	pertenece x ys 
	&& tienenLosMismosElementos xs ys
