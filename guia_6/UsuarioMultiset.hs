import Multiset

--O(s*m) donde s es el largo del string y m el costo de addMS
ocurrencias :: String -> MultiSet Char
ocurrencias [] = emptyMS
ocurrencias (x:xs) = addMS x (ocurrencias xs)
