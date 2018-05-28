-- vamos a juntarnos 

import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec -- Para poder usar los tests que se piden más abajo (ponerlo luego de instalar hspec!!)

data Día = Día { 
   año :: Int, 
   mes :: Int, 
   día :: Int 
} deriving Eq

data Invitado = Invitado { 
  nombre :: String, 
  calendario :: [ RestriccionParaJuntarse ] 
}
{-}
Además, ya tenemos una función cantidadDeDiasEnMes, que dado un número de mes y de año nos dice la cantidad de días que tiene ese mes:
> cantidadDeDiasEnMes 12 2017
=> 31-}

--1
--a 
type RestriccionParaJuntarse = Día -> Bool

--b
diasQuePuedeJuntarse :: Invitado -> [Día] -> [Día] 
diasQuePuedeJuntarse invitado dias = filter (puedeJuntarse invitado) dias

puedeJuntarse :: Invitado -> Día -> Bool
puedeJuntarse invitado = not . tieneResticciones (calendario invitado)

tieneResticciones :: [RestriccionParaJuntarse] -> Día -> Bool
tieneResticciones [] _ = False 
tieneResticciones (primerRestriccion : restoDeRestricciones) dia = primerRestriccion dia || tieneResticciones restoDeRestricciones dia

-- c. 
{-¿Sería posible determinar en qué días puede juntarse un invitado con una cantidad
infinita de restricciones? Justificar conceptualmente.

explicale asi como decis vos, si vos sabes que son dias asi numeros y todo
explicale de la misma forma
queda ciclando en el caso que todos sean falsos, que el tipo nunca pueda juntarse-}

--2 
--Declarar las siguientes funciones de modo que puedan usarse para generar restricciones
--para juntarse:

--a
tengoUnaCita :: Día -> RestriccionParaJuntarse
tengoUnaCita diaDeLaCita dia = (==) diaDeLaCita dia
-- si vos le pasas el dia de la cita, te devuelve una restriccion para juntarse
-- si es el mismo día, entonces el chabon no puede, si no es el mismo dia, el chabon puede

--b
esFeriado :: [Día] -> RestriccionParaJuntarse
esFeriado díasFeriados dia = elem dia díasFeriados

--c
podriaIrIndeseable :: Invitado -> RestriccionParaJuntarse
podriaIrIndeseable invitado día = puedeJuntarse invitado día

--d
cantidadDeDiasEnMes 12 2017 = 31

esFinDeMes :: RestriccionParaJuntarse
esFinDeMes unDia = resto5diasAlaCantidad unDia <= dia unDia

resto5diasAlaCantidad :: Día -> Int
resto5diasAlaCantidad día = (+) (-5) (cantidadDeDiasEnMes (día mes) (día año))

--e 
uhJustoTengoTurnoConElDentista :: RestriccionParaJuntarse
uhJustoTengoTurnoConElDentista _ = True

maria = invitado {
	nombre = "Maria",
	calendario = [uhJustoTengoTurnoConElDentista, esFinDeMes, esFeriado diasFeriados, podriaIrIndeseable invitado, tengoUnaCita diaDeLaCita]
}

diaDelaBandera = Día {
	día = 20,
	mes = 06,
	año = 2018
}

diaVeterano = Día {
	día = 02,
	mes = 04,
	año = 2018
}
diasFeriados = [diaDelaBandera, diaVeterano]

--3 
agendarCita :: Día -> Invitado -> Invitado
agendarCita día invitado = nuevoCalendario invitado día

nuevoCalendario unInvitado unDía = invitado {calendario = (tengoUnaCita dia) :  (calendario invitado)}

--4
--a

buscarElMejorDiaParaLaReunión :: [Invitado] -> Reunión -> Día
buscarElMejorDiaParaLaReunión invitados reunión = find (laMayorCantidadDeInvitados invitados) reunión

laMayorCantidadDeInvitados :: [Invitado] -> Día -> Bool
laMayorCantidadDeInvitados invitados día = puedeJuntarse head.invitados dia  || laMayorCantidadDeInvitados tail.invitados dia

--b

type Reunión = [Día]

confirmarReunión :: [Invitado] -> Reunión -> [Invitado]
confirmarReunión reunión invitados = map (agendarCita (buscarElMejorDiaParaLaReunión invitados reunión)) invitados

--5
maratónDeReuniones :: [Reunión] -> [Invitado] -> [Invitado]
maratónDeReuniones (primeraReunión: restoReuniones) = maratónDeReuniones restoReuniones . confirmarReunión primeraReunión 

maratónDeReuniones2 reuniones invitados = componerTodo (map confirmarReunión reuniones) invitados

componerTodo :: [a -> a] -> a -> a  
componerTodo = foldr (.) id


