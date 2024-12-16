module Library where
import PdePreludat

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Number,
    inventario :: [Material]    -- De un mismo material se pueden tener varias unidades.
} deriving(Show, Eq)

eze :: Personaje
eze = UnPersonaje "Ezequiel" 1000 ["sueter", "fogata", "pollo", "pollo"]
-- Y craftea un pollo asado, mantiene su sueter intacto, se queda con un sólo pollo, sin fogatas 
-- y pasa a tener un pollo asado en su inventario. Su puntaje pasa a ser 4000.

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Number,   -- en segundos
    producto :: Material
} deriving(Show, Eq)

type Material = String

-- Por ejemplo:
-- para hacer una fogata, se necesita madera y fósforo y se tarda 10 segundos
-- para hacer pollo asado, se necesita fogata y un pollo, pero se tarda 300 segundos
-- para hacer un sueter, se necesita lana, agujas y tintura, y demora 600 segundos

fogata :: Receta
fogata = UnaReceta ["madera", "fosforo"] 10 "fogata" 

polloAsado :: Receta
polloAsado = UnaReceta ["fogata", "pollo"] 300 "pollo asado"

sueter :: Receta
sueter = UnaReceta ["lana", "agujas", "tintura"] 600 "sueter"

-- 1) Hacer las funciones necesarias para que un jugador craftee un nuevo objeto
-- El jugador debe quedar con el nuevo objeto en su inventario
-- El jugador debe quedar sin los materiales usados para craftear
-- La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.
-- El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. 
-- En caso contrario, no se altera el inventario, pero se pierden 100 puntos.

--intentarCraftear :: Personaje -> Material -> Personaje
--intentarCraftear personaje objeto = puedeCraftear personaje objeto

intentarCraftear :: Receta -> Personaje -> Personaje
intentarCraftear receta personaje
    | tieneLosMaterialesRequeridosPara personaje receta = craftear receta personaje
    | otherwise                                         = perderPuntos 100 personaje

craftear :: Receta -> Personaje -> Personaje
craftear receta =  sumarPuntos (10 * tiempo receta). quitarMaterialesUtilizados (materiales receta) . agregarMaterial (producto receta)
-- 1ero. El jugador debe quedar con el nuevo objeto en su inventario
-- 2dos. El jugador debe quedar sin los materiales usados para craftear
-- 3ero. La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.

-- Funciones Auxiliares --

tieneLosMaterialesRequeridosPara :: Personaje -> Receta -> Bool
tieneLosMaterialesRequeridosPara personaje = all(tieneMaterial personaje) . materiales

tieneMaterial :: Personaje -> Material -> Bool
tieneMaterial personaje material = material `elem` inventario personaje

perderPuntos :: Number -> Personaje -> Personaje
perderPuntos puntosAPerder personaje = personaje {puntaje = puntaje personaje - puntosAPerder}

sumarPuntos :: Number -> Personaje -> Personaje
sumarPuntos puntosAGanar personaje = personaje {puntaje = puntaje personaje + puntosAGanar}

agregarMaterial :: Material -> Personaje -> Personaje
agregarMaterial material personaje = personaje {inventario = material : inventario personaje}

--eliminarMaterial :: Material -> Personaje -> Personaje
--eliminarMaterial material personaje = personaje {inventario = filter (/= material) (inventario personaje)}

-- Implemente esto porque de un mismo material se puede tener varias unidades, entonces solo quiero eliminar una de esas unidades

quitarUnaVez :: Eq a => a -> [a] -> [a]
quitarUnaVez _ [] = []
quitarUnaVez material (m:ms) 
    | material == m = ms
    | otherwise     = m:quitarUnaVez material ms

--eliminarObjetosUtilizados :: [Material] -> Personaje -> Personaje
--eliminarObjetosUtilizados materiales personaje = foldr eliminarMaterialUnaVez personaje materiales

quitarMaterial :: Material -> Personaje -> Personaje
quitarMaterial material personaje = personaje {inventario = quitarUnaVez material (inventario personaje)}

quitarMaterialesUtilizados :: [Material] -> Personaje -> Personaje
quitarMaterialesUtilizados materiales personaje = foldr quitarMaterial personaje materiales

-- Fin de funciones auxiliares del punto 1) --