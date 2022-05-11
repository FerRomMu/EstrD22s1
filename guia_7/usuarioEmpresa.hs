import Empresa

comenzarCon :: [SectorId] -> [CUIL] -> Empresa

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

esComodin :: CUIL -> Empresa -> Bool
