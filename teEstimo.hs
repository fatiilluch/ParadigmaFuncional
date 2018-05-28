{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec -- Para poder usar los tests que se piden más abajo (ponerlo luego de instalar hspec!!)

type Nombre = String
type Sentimiento = String
type Estrategia = Persona -> Persona

data Persona = Vendedor Nombre Estrategia [Sentimiento] | Comprador Nombre [Sentimiento] deriving Show

jorgito = Comprador "Jorge" ["molestia"]
tincho = Comprador "Martín" ["indiferencia", "molestia"]
agus = Comprador "Agustín" ["felicidad"]
pepe = Comprador "Jose" ["indiferencia", "molestia", "felicidad", "ira asesina"]

lucas = Vendedor "Lucas" gauchada ["felicidad"]
pato = Vendedor "Patricio" (fraudeOlímpico . niFuNiFa) ["bronca", "felicidad"]
flor = Vendedor "Florencia" (estafa . fraudeOlímpico) []
nacho = Vendedor "Ignacio" (estafa . seguidilla 10 . niFuNiFa) ["molestia"]

-- 1a
sentir :: Sentimiento -> Estrategia
sentir sentimiento (Vendedor nombre estrategia sentimientos) = Vendedor nombre estrategia (sentimiento : sentimientos)
sentir sentimiento (Comprador nombre sentimientos) = Comprador nombre (sentimiento : sentimientos)

-- 1b
gauchada :: Estrategia
gauchada = sentir "satisfacción"

niFuNiFa :: Estrategia
niFuNiFa = sentir "indiferencia"

-- 1c
estafa :: Estrategia
estafa = sentir "bronca" . sentir "felicidad"

fraudeOlímpico :: Estrategia 
fraudeOlímpico = sentir "ira asesina" . estafa

-- 1d 
seguidilla :: Int -> Estrategia
seguidilla 0 = id
seguidilla n = sentir "molestia" . seguidilla (n-1)

-- 2 
sentimientosMalos = ["molestia", "bronca", "ira asesina"]

-- 2a) 
seSienteBien :: Persona -> Bool
seSienteBien = comoSeSiente . sentimientos

comoSeSiente :: [Sentimiento] -> Bool
comoSeSiente = not.any (flip elem sentimientosMalos)

sentimientos :: Persona -> [Sentimiento]
sentimientos (Vendedor _ _ susSentimientos) = susSentimientos
sentimientos (Comprador  _ susSentimientos) = susSentimientos

-- 2b)
seSienteMal :: Persona -> Bool
seSienteMal = not . seSienteBien

-- 2c) 
quiereMatarATodos :: Persona -> Bool
quiereMatarATodos persona = seSienteMal persona && sienteAlMenos3CosasDiferentes persona && iraAsesina persona

elementosÚnicos :: [Sentimiento] -> [Sentimiento]
elementosÚnicos [] = []
elementosÚnicos (x:xs) 
 | (not . elem x) xs = x : elementosÚnicos xs
 | otherwise = elementosÚnicos xs

sienteAlMenos3CosasDiferentes :: Persona -> Bool
sienteAlMenos3CosasDiferentes = (>2)  . length . elementosÚnicos . sentimientos

iraAsesina :: Persona -> Bool
iraAsesina = elem "ira asesina" . elementosÚnicos . sentimientos

agradecimiento :: Estrategia
agradecimiento = sentir "placer"

defensaAlConsumidor :: Estrategia
defensaAlConsumidor = sentir "tristeza" . sentir "miedo"

juicio :: Int -> Estrategia
juicio cantidadDeAbogados = sentir "depresión" . seguidilla cantidadDeAbogados

reaccionar :: Persona -> Persona -> Estrategia 
reaccionar comprador vendedor 
 | quiereMatarATodos (compradorLuegoDeCompra vendedor comprador) = juicio ((length.sentimientos) comprador)
 | seSienteBien (compradorLuegoDeCompra vendedor comprador) = agradecimiento
 | otherwise = defensaAlConsumidor

compradorLuegoDeCompra :: Persona -> Estrategia
compradorLuegoDeCompra vendedor comprador = estrategia vendedor comprador

estrategia :: Persona -> Estrategia
estrategia (Vendedor _ susEstrategias _) = susEstrategias

-- 4a
sientePlacerLuegoDeVenta :: Persona -> Persona -> Bool
sientePlacerLuegoDeVenta = luegoDeVenderSiente "placer"

luegoDeVenderSiente :: Sentimiento -> Persona -> Persona -> Bool
luegoDeVenderSiente sentimiento comprador = (==) sentimiento . head . sentimientos . venderYRecibirReacciónDe comprador

venderYRecibirReacciónDe :: Persona -> Persona -> Persona
venderYRecibirReacciónDe comprador vendedor = reaccionar comprador vendedor vendedor

-- 4b Definir la función sienteDepresiónLuegoDeVenta.

sienteDepresiónLuegoDeVenta = luegoDeVenderSiente "depresión"

--4c 

sienteTristezaLuegoDeVenta = luegoDeVenderSiente "tristeza"

--4d

Realizar una Venta Elite: a partir de un conjunto de vendedores, seleccionar únicamente los que 
sientan placer luego de venderle a cierta persona. Después,
averiguar cómo se sentiría cada uno de esos vendedores luego de hacer la venta.
> ventaElite agus [lucas, pato, flor, nacho]
[Vendedor " Lucas " <function> [" placer ","felicidad"]] → Porque sólo Lucas sentiría placer
luego de venderle a Agus. Y lo mostramos tal como quedaría luego de venderle.


ventaElite comprador = map (venderYRecibirReacciónDe
comprador) . filter (sientePlacerLuegoDeVenta comprador)



{-}
-- 5a
f w x y z = map (x w) . filter (y . z)
f :: n -> (n -> a -> d) -> (b -> Bool) -> (a -> b) -> [a] -> [d]

-- x w es la funcion que recibe map. y esta funcion recibe un valor (w) y un a que es elemento de la lista que le pasa filter 
-}