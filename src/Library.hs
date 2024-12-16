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

-- 2) Dado un personaje y una lista de recetas:

-- 2.a) Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje.

recetasCrafteablesYDuplicadorasDePuntaje :: Personaje -> [Receta] -> [Receta]
recetasCrafteablesYDuplicadorasDePuntaje personaje = filter (\receta -> puedeCraftear personaje receta && duplicaSuPuntaje personaje receta)

recetasCrafteablesYDuplicadorasDePuntaje' :: Personaje -> [Receta] -> [Receta]
recetasCrafteablesYDuplicadorasDePuntaje' personaje = filter (esCrafteableYDuplicadora personaje)

esCrafteableYDuplicadora :: Personaje -> Receta -> Bool
esCrafteableYDuplicadora personaje receta = puedeCraftear personaje receta && duplicaSuPuntaje personaje receta

puedeCraftear :: Personaje -> Receta -> Bool
puedeCraftear = tieneLosMaterialesRequeridosPara 

duplicaSuPuntaje :: Personaje -> Receta -> Bool
duplicaSuPuntaje personaje = (>= 2 * puntaje personaje) . puntaje . flip intentarCraftear personaje

-- 2.b) Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldr intentarCraftear -- tiene que ser foldr por el tipado de craftear (b -> a -> a) -> a -> [b] -> a
--                                                                                      (Receta -> Persona -> Persona) -> Persona -> [Receta] -> Persona    

-- 2.c) Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés.

-- Interpreta mal el enunciado xd
--quedaConMasPuntos :: Personaje -> [Receta] -> Bool
--quedaConMasPuntos personaje recetas = quedaConMasPuntosNormal personaje recetas || quedaConMasPuntosAlReves personaje recetas

-- Aca lo hice bien :)

puntajeOrdenNormal :: Personaje -> [Receta] -> Number
puntajeOrdenNormal personaje = puntaje . craftearSucesivamente personaje

quedaConMasPuntosAlReves :: Personaje -> [Receta] -> Bool
quedaConMasPuntosAlReves personaje recetas = ((> puntajeOrdenNormal personaje recetas) . puntaje . craftearSucesivamente personaje . reverse ) recetas

-- PARTE 2 . MINE --

data Bioma = UnBioma {
    nombreBioma :: String,
    materialesBioma :: [Material],
    elementoNecesario :: Material -- Para poder minar en un bioma particular el personaje debe tener un elemento necesario según el bioma
} deriving(Show , Eq)

--Por ejemplo, en un bioma ártico, donde hay hielo, iglues y lobos, se debe tener un suéter. 
artico :: Bioma
artico = UnBioma "artico" ["hielo", "iglu", "lobo"] "sueter"

-- 1) Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje.
-- Cosas a tener en cuenta :
-- Cuando un personaje va a minar a un bioma, si cuenta con el elemento necesario, agrega a su inventario uno de los materiales
-- del bioma y gana 50 puntos
-- La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar. 
-- En caso de no poder minar por no tener lo necesario el personaje se va con las manos vacías y sigue como antes.

intentarMinar :: Herramienta -> Bioma -> Personaje -> Personaje
intentarMinar herramienta bioma personaje 
    | tieneElementoNecesario bioma personaje = minar herramienta bioma personaje
    | otherwise                              = personaje 

minar :: Herramienta -> Bioma -> Personaje -> Personaje
minar herramienta bioma personaje = (flip agregarMaterial personaje . herramienta . materialesBioma) bioma 

-- Por ejemplo, si un personaje con un sueter en su inventario mina el artico con un pico de precisión 1, 
-- agrega un iglú a su inventario.

ezequielito :: Personaje
ezequielito = UnPersonaje "ezequielito" 1000 ["sueter"]

-- Funciones auxiliares --

tieneElementoNecesario :: Bioma -> Personaje -> Bool
tieneElementoNecesario bioma personaje = ((`elem` inventario personaje) . elementoNecesario) bioma

-- Una herramienta me queda comodo que sea dado una lista de materiales, me devuelva un material
type Herramienta = [Material] -> Material

-- El hacha hace que se mine el último de los materiales del bioma
hacha :: Herramienta
hacha = last 

-- Mientras que la espada actúa sobre el primero de ellos
espada :: Herramienta
espada = head

-- Existe tambien el pico, que por ser más preciso permite apuntar a una determinada posición de los materiales. 
pico :: Number -> Herramienta
pico = flip (!!)  


-- 2) Definir las herramientas mencionadas y agregar dos nuevas. Mostrar ejemplos de uso. 
-- Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la función minar.

-- NUEVA HERRAMIENTA -> Existe un palo, que agarra el material que se encuentre en la posicion media de la lista de materiales
palo :: Herramienta
palo materiales = (flip pico materiales . (`div` 2) . length) materiales 

-- NUEVA HERRAMIENTA --> Existe una hoz, que agarra al material del nombre mas grande (segun abecedario)
hoz :: Herramienta
hoz = maximum

-- 2.a) Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales.

posicionDelMedio :: [Material] -> Material
posicionDelMedio materiales = ((!!) materiales . (`div` 2) . length) materiales

-- 2.b) Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores

--conLambda :: Herramienta
--conLambda materiales = (\material -> (maximum . length) material) materiales

-- 3) ¿Qué pasa al intentar minar en un bioma con infinitos materiales? 
-- Mostrar ejemplos donde con diferentes herramientas o personajes sucedan diferentes cosas. Justificar.

biomaInfinito :: Bioma
biomaInfinito = UnBioma "infinito" listaInfinitaDePollos "sueter"

listaInfinitaDePollos :: [String]
listaInfinitaDePollos = "pollo" : listaInfinitaDePollos

-- CASOS

-- 1ER CASO
-- > minar espada biomaInfinito ezequielito
-- ezequielito ahora en su inventario tienen un "sueter" y un "pollo". 

-- Esto es posible gracias a la evaluacion perezosa 
-- (lazy evaluation) que utliza Haskell. La cual nos permite operar/trabajar con listas infinitas llegando a un resultado
-- (es esta caso obtener la cabeza de la lista) sin necesidad de saber la lista (el parametro) en su completitud. Esto es 
-- porque justo la funcion head simplemente utiliza el primer elemento de la lista, ignorando la cola. Por lo tanto, en esta
-- caso no fue necesario calcular/obtener toda la lista para realizar un head de la misma

-- 2DO CASO
-- > minar hacha biomaInfinito ezequielito
-- EXPLOTO TODO!! ERROR

-- Este caso no es posible porque la herramienta hacha agarra el ultimo elemento de la lista, por lo tanto aunque tenga una
-- evaluacion perezosa si o si es necesario obtener o llegar al final de la lista para agarra dicho elemento final. Pero como 
-- la lista es infinita por lo tanto NUNCA voy a llegar xd. Entonces, como conclusion de este caso, podemos observar que 
-- lazy evaluation NO es magico o NO siempre voy a llegar a un resultado cuando opero con listas infinitas!!

-- 3ER CASO
-- > minar (pico 2) biomaInfinito ezequielito
-- ezequielito ahora en su inventario tienen un "sueter" y un "pollo". 

-- Similar al 1er caso, solo que en este la herramienta (pico 2) necesita acceder al indice correspondiente, por lo tanto haskell
-- va a "generar"/obtener la lista hasta donde sea necesario para el pico, que en esta caso hasta el indice, es decir una lista de 
-- 3 elementos. En este caso tampoco fue necesairo obtener/calcular toda la lista para operar con ella!!