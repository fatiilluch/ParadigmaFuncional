-- tercera guerra mundial
{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions -- Para mostrar <Function> en consola cada vez que devuelven una
import Data.List -- Para métodos de colecciones que no vienen por defecto (ver guía de lenguajes)
import Data.Maybe -- Por si llegan a usar un método de colección que devuelva “Just suElemento” o “Nothing”.
import Test.Hspec -- Para poder usar los tests que se piden más abajo (ponerlo luego de instalar hspec!!)

-- Los países

type Estrategia = País -> País

data País = País {
    nombre :: String,
    recursos :: Float,
    enemigos :: [País],
    aliados :: [País],
    estrategia :: Estrategia
} deriving Show

eeuu = País{
    nombre = "Estados Unidos",
    recursos = 1500.00,
    enemigos = [afganistán, coreaNorte, china],
    aliados = [alemania, inglaterra],
    estrategia = bombas 3 . influenciar
}

rusia = País{
    nombre = "Rusia",
    recursos = 2100.00,
    enemigos = [turquía, eeuu],
    aliados = [],
    estrategia = bombas 10 . complot . (virus "Witzelsucht") 
}

-- Lo de acá abajo es para que no se rompa al probar en GHCi nada más:
paísGenérico = País "" 1000 [] [] (bombas 1)

afganistán = paísGenérico {nombre = "Afganistán"}
coreaNorte = paísGenérico {nombre = "Corea del Norte"}
china = paísGenérico {nombre = "China"}
alemania = paísGenérico {nombre = "Alemania"}
inglaterra = paísGenérico {nombre = "Inglaterra"}
turquía = paísGenérico {nombre = "Turquía"}

-- Las estrategias
influenciar país = país {aliados = []}

bombas cantidad pais = pais { recursos = (recursos pais) - ( cantidad *  ( (0.1) * (recursos pais))) } 

virus nombre pais 
 | menosDeVeinte nombre =  pais {recursos = (recursos pais) - 100} 
 |otherwise = pais {recursos = 0} 

menosDeVeinte = (<20) . length 

complot país = país {enemigos = map aumentarRecursos (enemigos país)}

aumentarRecursos país = país {recursos = (recursos país) + 1000}

-- 3 

granGuerra país = contraAtaqueEnemigo país {enemigos = map (estrategia país) (enemigos país)}

contraAtaqueEnemigo país = foldl (flip ($)) país (estrategiasEnemigas país)

estrategiasEnemigas país = map estrategia (enemigos país)

-- 4

ganoGuerra país = recursosAceptables país && ricoEnRecursos país

recursosAceptables país = ((>500).recursos.granGuerra) país

ricoEnRecursos país = (recursos.granGuerra) país > maximoRecursoEnemigo país

maximoRecursoEnemigo país = (maximum.(map recursos) .enemigos.granGuerra) país 
