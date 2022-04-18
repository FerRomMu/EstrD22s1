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

