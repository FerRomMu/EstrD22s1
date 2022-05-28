--Usuario de Organizador:

--ACLARACION SOBRE COSTOS:
--A lo largo de toda la resolución las siguientes letras referenciaran:
--C: La cantidad de programas en la organizacion
--P: La cantidad de personas en la organizacion

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--precondicion: Las personas dadas trabajaron en la organizacion
programasEnComun p1 p2 o =
  union (programasDe o p1) (programasDe o p2)
--O(log P + C log C) en peor caso.
--JUSTIFICACION: log P en el peor caso por programasDe y C log C en el
--peor caso por union de ambos sets.

esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker o p = esAutorDeTodos p (todosLosProgramas o)
--O(C * (log C + log P))
--JUSTIFICACION: todosLosProgramas o es costo C lo que es menospreciable
--respecto a la eficiencia de esAutorDeTodos que es C*(logC+logP) en donde
--siempre recorrera C elementos por ser todos los programas.

esAutorDeTodos :: Persona -> [Checksum] -> Bool
esAutorDeTodos p [] = True
esAutorDeTodos p (c:cs) = belongs p (autoresDe c) && esAutorDeTodos p cs
--O(C * (logC + log P)) 
--JUSTIFICACION: Se recorreran hasta C elementos y para cada uno
--de ellos se aplicaran tanto belongs (que depende de P) como autoresDe(que
--depende de C).

----TAD ORGANIZADOR

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
-- *Todas las personas que son key en el map persona set checksum, son values en
--algún set del map checksum. A su vez los checksum que son key del map Checksum
--set Persona, son values en algún set del map Persona set Checksum. Así como tambien
--estos se corresponden reciprocamente (si para un determinado checksum hay un
--determinado set de Personas, para todas esas Personas cuando son key del otro map
--tienen dentro de su set ese checksum y viceversa)
-- *Si la organizacion esta vacía ambos maps tambien lo estan.

nuevo :: Organizador
nuevo = MkO emptyM emptyM
--O(1)
--Las eficiencia de emptyM es de O(1).

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (Mk0 mc mp) c sp =
  Mk0 (assocM c sp mc) (agregarAAutores c (set2List sp) mp)
--O(P*(log C + log P)+ log C)
--JUSTIFICACION: Porque el assocM depende de C por lo que es O(log C)y a su
--vez agregarAAutores, si bien depende de ambos, por cada P aumentara su coste.
--Por ello queda O(P*(log C + log P) + log C)

agregarAAutores :: Checksum -> [Persona] -> Map Persona (Set Checksum)
agregarAAutores c [] mp = mp
agregarAAutores c (p:ps) mp =
  assocM p setActualizado mp
  where setActualizado = addS c (fromJust (lookupM p mp))
--O(P*(log C + log P))
--JUSTIFICACION: Por cada persona en la lista dada se realizará un lookupM que 
--depende de la cantidad de personas y un addS que depende de la cantidad de programas
--en el set.

todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mc mp) = domM mc
--O(C)
--JUSTIFICACION: Porque obtener el domM de mc es O(C).

autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mc mp) c = fromJust (lookupM c mc)
--O(log C)
--JUSTIFICACION: Porque obtener el value del map de checksum es O(log C).

programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO mc mp) p = fromJust (lookupM p mp)
--O(log P)
--JUSTIFICACION: Porque obtener el value del map de personas es O(log P).

programaronJuntos :: Organizador -> Persona -> Persona -> Bool
programaronJuntos (MkO mc mp) p1 p2 =
  estaAlgunoEnAmbos (domM mc) setDeP1 setDeP2
  where
    setDeP1 = fromJust (lookupM p1 mp)
    setDeP2 = fromJust (lookupM p2 mp)
--O(Log P + C*log C)
--JUSTIFICACION: Porque por un lado se debe buscar en los maps de personas los
--sets de los respectivos checksums, siendo O(log P) y por otro lado se debe usar
--estaAlgunoEnAmbos que es O(C*logC)

estaAlgunoEnAmbos :: [Checksum] -> Set Checksum -> Set Checksum
estaAlgunoEnAmbos [] s1 s2 = False
estaAlgunoEnAmbos (c:cs) s1 s2 = (belongs c s1 && belongs c s2)
  || estaAlgunoEnAmbos cs s1 s2
--O(C*log C)
--JUSTIFICACION: Por cada programa hay que usar un belongs de O(log c).

nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO mc mp) p =
  let susProgramas = fromJust (lookupM p mp)
  in
    sizeS susProgramas
--O(log P)
--JUSTIFICACION: En el peor de las casos el buscas el set de programas hechos
--por la persona dada en el map de personas será log p, mientras que al ser O(1)
--hacer sizeS es menospreciable.


