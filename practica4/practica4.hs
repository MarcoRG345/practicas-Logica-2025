module Practica04 where
-- Práctica 04: Algoritmo DPLL

-- 3. Desarrollo de la Práctica

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)


-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Sinónimo Literal
type Literal = Prop

-- Sinónimo Cláusula
type Clausula = [Literal]


-- 3.1 Implementación del algoritmo DPLL (Parte 1)

-- Interpretación
type Interpretacion = [(String, Bool)]

-- Estado del Algoritmo
type Estado = (Interpretacion, [Clausula])

-- Ejercicios

-- Conflicto
conflict :: Estado -> Bool
conflict (x, []) = False --no hay conflicto si no hay clausulas
conflict (x, []:_) = True --conflicto si está la clausula vacia
conflict (x, _:ys) = conflict (x, ys) --caso recursivo

-- Éxito
success :: Estado -> Bool
success (xs, []) = True --exito si no quedan clausulas por stisfacer
success (xs, ys) = False

-- Funcion aux que regresa la longitud de una lista
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

-- Funcion aux repr de una variable proposicional a su interpretacion
repr :: Literal -> Interpretacion
repr (Var l) = [(l, True)]
repr (Not (Var l)) = [(l, False)]
-- Funcion aux elimina literales de un conjunto de clausulas.

elimLiterals :: [Clausula] -> [Clausula]
elimLiterals [] = [] -- Caso base: si es vacia, no hay mas que buscar.
elimLiterals (x:xs) = if long x == 1 then elimLiterals xs else x:elimLiterals xs -- Caso recursivo: Busca aquellas clausulas de longitud 1 y eliminalas si no regresa el cons del elemento con el resto de clausulas.

-- Cláusula unitaria
unit :: Estado -> Estado
unit (xs, (y:ys)) = amplifica xs (y:ys) -- Amplifica la busqueda entre clausulas con la recursion en el conjunto de clausulas.
  where
      amplifica :: Interpretacion -> [Clausula] -> Estado
      amplifica xs [] = (xs, elimLiterals (y:ys)) -- Caso base: Si el cconjunto de clausulas es vacia no hay que buscar, elimina las literales.
      amplifica xs (z:zs) = if long z == 1 then amplifica (xs ++ repr (head z)) zs else  amplifica xs zs -- Caso recursivo: busca aquelas clausulas con longitud 1 en el conjunto de clausulas y regresa el formato adecuado de su interpretacion.

-- Funcion aux que verifica si una interpretación está contenida en una cláusula
contiene :: (String, Bool) -> Clausula -> Bool
contiene _ [] = False -- Caso base: No se encontro coincidencia, por lo que no se encuentra la literal
contiene (p, True) (x:xs) = if (Var p) ==  x then True else contiene (p, True) xs -- Caso rec: con interp verdadera verifica que sea igual a la literal si no sigue buscando.
contiene (p, False) (x:xs) = if (Not (Var p)) == x then True else contiene (p, False) xs -- Caso rec: con interp falsa verifica que sea igual a la literal si no sigue buscando.

-- Eliminación 
elim :: Estado -> Estado
elim ([], ys) = ([], ys) --Caso Base: si es vacia no hay nada que eliminar
elim ((x:xs), ys) = amplifica (x:xs) ys -- Amplififca la busqueda entre clausulas con la recursion en la interp.
  where
    amplifica :: Interpretacion -> [Clausula] -> Estado
    amplifica [] ys = ((x:xs), ys) -- Caso base: La interpretacion es vacia, regresa la interp sin hacer rec y su conjunto de clausulas.
    amplifica (x:xs) ys = amplifica xs [k | k<-ys, contiene x k == False] -- Caso recursivo: para cada interp fija conserva aquellas que no estan presentes en el conjunto de clausulas.

-- Funcion aux que simplifica una clausula dado el complemento de una literal.
simplifica :: (String, Bool) -> Clausula -> Clausula
simplifica _ [] = [] -- Caso base: Regresa la vacia si no hay mas literales en la clausula
simplifica (p, True) (x:xs) = if (Not (Var p)) == x then simplifica (p, True) xs else x:simplifica (p, True) xs -- Caso recursivo, si la p es verdadera, entonces busca su coincidencia en su complemento.
simplifica (p, False) (x:xs) = if (Var p) == x then simplifica (p, False) xs else x:simplifica (p, False) xs -- Caso recursivo, si la p es falsa , entonces busca su coincidencia en su complemento.
  
-- Reducción 
red :: Estado -> Estado
red ([], ys) = ([], ys) --Caso Base: si no hay interp no se hace ninguna red y se devuelve el mismo estado sin cambios
red ((x:xs), ys) = amplifica (x:xs) ys -- Amplifica la busqueda entre las clausulas con la recursion en la interp.
  where
    amplifica :: Interpretacion -> [Clausula] -> Estado
    amplifica [] ys = ((x:xs), ys) -- Caso base: Regresa el modelo como esta si el modelo actual es [].
    amplifica (x:xs) ys = amplifica xs [simplifica x k | k<-ys] -- Caso recursivo: Para culquier clausula simplifica cada una de sus interpretaciones.
    
-- Separación
sep :: Literal -> Estado -> (Estado, Estado)
sep (Var p) (xs, ys) = (((xs ++ repr (Var p)), ys), ((xs ++ repr (Not (Var p))), ys)) -- Para una literal sin negacion regresa dos estados, su negado en el formato "repr" correspondiente y su negado.
sep (Not (Var p)) (xs, ys) = (((xs ++ repr (Not (Var p))), ys), ((xs ++ repr (Var p)), ys)) -- Mismo caso solo que al reves.


-- 3.2 Árboles DPLL

data ArbolDPLL = 
    Node Estado ArbolDPLL |
    Branch Estado ArbolDPLL ArbolDPLL |
    Void 

--Función aux que devuelve el segundo elem de una tupla
segundoElemento :: (a, b) -> b
segundoElemento (_,y) = y

--Función que construye el arbolDPLL
construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado
    | conflict estado = Node estado Void --si hay conflicto construimos un nodo con el estado pero se para
    | success estado = Node estado Void --si hay succes tamb construimos un nodo y ya no se expande más
    | segundoElemento propuesto /= segundoElemento estado = Node estado (construirArbolDPLL propuesto) --si aplicamos, unit, red, elim y cambia ent seguimos con el nuevo "propuesto"
    | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der) --si no avanza con unit ent se crea una rama izq y una derecha
    where
        propuesto = red (elim (unit estado)) --aplicamos unit, elim, red, en ese orden
        (izq, der) = sep (heuristicsLiteral (segundoElemento estado)) estado --elegimos una literal y separamos en izq y derecha y asumimos una como verdadera y la otra como falsa

--Función que explora el arbol y busca que sea satisfacible
explorarArbolDPLL :: ArbolDPLL -> Estado -> Estado
explorarArbolDPLL Void estado = estado --si llega a arbol vacio devuelve el estado que tenia hasta ese punto
explorarArbolDPLL (Node estado1 t) _ = explorarArbolDPLL t estado1 --en el nodo continua explorando el subarbol t usando como nuevo estado a "estado1"
explorarArbolDPLL (Branch estado1 t1 t2) _ =  --si tiene el estado1 y dos subarboles t1 t2 sigue explroando
    if conflict x --vemos si es conflict
        then explorarArbolDPLL t2 estado1 --si sí hay conflicto ent explroamos t2 usando el mismo estado de donde venimos estado1
        else x --si no hay conflicto en la rama izq, ent es valido y manda el resultado
        where x = explorarArbolDPLL t1 estado1 --explorar recursivamente t1 con estado1

-- 3.3 Implementación del algoritmo DPLL (Parte 2)

--Función aux que determina si es vacia
esVacia :: [a] -> Bool
esVacia [] = True --si es vacia devuelve true
esVacia _  = False --si no devuelve false

--Función aux que obtiene el nombre de una variable sin importar si está negada
getNombre :: Literal -> String
getNombre (Var nombre) = nombre --extrae y devuelve el nombre
getNombre (Not (Var nombre)) = nombre --extrae y duelve solo el nombre sin el not

-- Heurística
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral clausulas =
    let vars = obtenerVariables clausulas --obtener variables unicas
        conteos = contarApariciones vars clausulas --contar apariciones
        varMax = buscarMaximo conteos --encontrar la variable que mas se repite
    in if esVacia vars
       then Cons False --Caso Base, no hay variables
       else Var varMax --si si ent devuelve la variable que más se repite
  where
    -- Función para obtener todas las variables únicas de las cláusulas
    obtenerVariables :: [Clausula] -> [String]
    obtenerVariables [] = []
    obtenerVariables (c:cs) = agregarVariablesDeClausula c (obtenerVariables cs) -- c clausula, cs clausulas 

    --Función que agrega variables de una cláusula a una lista para evitar duplicados
    agregarVariablesDeClausula :: Clausula -> [String] -> [String]
    agregarVariablesDeClausula [] vars = vars
    agregarVariablesDeClausula (lit:ls) vars = --lit literal, ls literales
        let nombre = getNombre lit
        in if pertenece nombre vars
           then agregarVariablesDeClausula ls vars
           else nombre : agregarVariablesDeClausula ls vars
    
    -- Función aux. para verificar si un elemento está en la lista
    pertenece :: String -> [String] -> Bool
    pertenece _ [] = False --Caso Base, si es vacia devuelve false
    pertenece x (y:ys) = x == y || pertenece x ys --si no tomamos el primer elemento de la lista y, y el resto de la lista ys
    --compara en x==y y si son iguales devuelve true y si no sigue buscando en x ys
    
    --FUnción aux. cuenta cuantas veces está cada variable en las claúsulas
    contarApariciones :: [String] -> [Clausula] -> [(String, Int)]
    contarApariciones [] _ = [] --CAso Base, si no hay vars ent no hay nada que contar
    contarApariciones (v:vs) cs = --Para cada variable v, cuenta cuántas veces aparece en la lista de cláusulas cs usando contarEnClausulas.
        (v, contarEnClausulas v cs) : contarApariciones vs cs
    --y continua recursivamente con el resto de las variables vs

    --FUnción aux. que cuenta cuantas veces está una variable en todas las claúsulas
    contarEnClausulas :: String -> [Clausula] -> Int
    contarEnClausulas _ [] = 0 --Caso base: si no hay cláusulas regresamos 0
    contarEnClausulas v (c:cs) = --contamos cuántas veces aparece la variable v en la primera cláusula c
        contarEnClausula v c + contarEnClausulas v cs
    --y le sumamos cuántas veces aparece en el resto de las cláusulas cs
    
    --Función aux. que cuenta cuantas veces está una variable en una claúsula
    contarEnClausula :: String -> Clausula -> Int
    contarEnClausula _ [] = 0 --Caso base: si la cláusula está vacía regresamos 0
    contarEnClausula v (lit:ls) = --contamos cuántas veces aparece la variable v en la primera cláusula c
        if getNombre lit == v
            then 1 + contarEnClausula v ls
            else contarEnClausula v ls
    --y le sumamos cuántas veces aparece en el resto de las cláusulas cs

    --FUnción aux. que busca la variable con máximo conteo
    buscarMaximo :: [(String, Int)] -> String
    buscarMaximo [] = "" --Caso base: si la lista está vacía, regresamos una cadena vacía
    buscarMaximo [(v, _)] = v --Caso base: si solo hay un elemento en la lista, ese es el máximo
    buscarMaximo ((v1, n1):resto) = --separamos la cabeza de la lista (v1, n1) y el resto de los pares
        let v2 = buscarMaximo resto --v2 es la variable que más aparece en el resto de la lista
            n2 = buscarConteo v2 resto --n2 es el número de veces que aparece esa variable v2 en el resto de la lista
        in if n1 >= n2 --comparamos cuántas veces aparece la variable actual v1 contra la mejor del resto v2
            then v1 --si v1 aparece más o igual que v2, entonces v1 es el nuevo máximo
            else v2  --si no, mantenemos v2 como el máximo

    --Función aux que busca el conteo de una var especifica
    buscarConteo :: String -> [(String, Int)] -> Int
    buscarConteo _ [] = 0 --si está vacía, no se encontró la variable, regresamos 0
    buscarConteo v ((v2, n):resto) = --se descompone la lista en la cabeza (v2, n) y la cola (resto)
        if v == v2 --si la variable actual es igual a la que estamos buscando:
        then n --se retorna el conteo asociado
        else buscarConteo v resto --sino, se sigue buscando en el resto de la lista

-- Verifica si una interpretación es consistente (sin contradicciones)
interpretacionConsistente :: Interpretacion -> Bool
interpretacionConsistente [] = True --Una interpretación vacía es consistente
interpretacionConsistente ((x,v):xs) = -- Si el elemento actual contradice algo en el resto, es inconsistente
    if contradice x v xs
        then False -- Se encontró una contradicción
        else interpretacionConsistente xs --Si no, sigue evaluando el resto
  where
    -- Función auxiliar que verifica si una variable x con un valor v
    -- aparece en la lista con un valor contradictorio
    contradice :: String -> Bool -> Interpretacion -> Bool
    contradice _ _ [] = False --No hay más elementos ent no hay contradicción
    contradice x v ((y,w):ys) =
        if x == y --Si la variable es la misma:
        then if v == w -- :y el valor es igual ent seguimos buscando
                then contradice x v ys 
                else True -- :pero si el valor es diferente ent hay contradicción
        else contradice x v ys --si es otra variable ent seguimos buscando

-- Función prinicipal
dpll :: [Clausula] -> Interpretacion
dpll clausulas = 
    let estadoInicial = ([], clausulas) --Estado inicial: sin ninguna variable asignada, con todas las cláusulas pendientes de evaluar
        arbol = construirArbolDPLL estadoInicial --Construye el árbol de decisión a partir del estado inicial (posibles asignaciones)
        (modelo, _) = explorarArbolDPLL arbol estadoInicial --Explora el árbol para encontrar un modelo que haga verdadera la fórmula
    in if success (modelo, []) && interpretacionConsistente modelo --Verifica si el modelo encontrado satisface todas las cláusulas y es consistente
        then modelo --Si cumple, retorna la interpretación encontrada
        else [] --Si no hay solución válida retorna vacio

--- Funciones auxiliares usadas en la practica pasada ---

-- Funcion aux que dado una formula en PROP devuelve su FNN

fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons a) = Cons a
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = And (fnn p) (fnn (Not q))
fnn (Not (Syss p q)) = fnn (Syss (Not p) (q))
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

-- Función para distribuir y pasarla a su forma normal conjuntiva
distribuir :: Prop -> Prop -> Prop
distribuir (And p q) r = And (distribuir p r) (distribuir q r)
distribuir p (And q r) = And (distribuir p q) (distribuir p r) 
distribuir p q = Or p q
-- Funcion para convertir una formula a FNC
fnc :: Prop -> Prop
fnc prop = fncdos (fnn prop) --Primero convertimos a fnn y ya luego a fnc
  where
    fncdos :: Prop -> Prop
    fncdos (And p q) = And (fncdos p) (fncdos q) --Conjunción recursiva
    fncdos (Or p q) = distribuir (fncdos p) (fncdos q)  --Dsitribuimos or sobre and
    fncdos p = p 

noRepetidos :: Clausula -> Clausula
noRepetidos [] = []
noRepetidos (x:xs) = x : noRepetidos [k | k <- xs, k /= x]

--Función para concatenar listas de claúsulas
concatenar :: [[c]] ->[c]
concatenar [] = []
concatenar (xs:xss) = xs ++ concatenar xss --concatenación recursiva

--Función para unir dos cláusulas sin repeticiones
clausulaUnion :: [Clausula] -> [Clausula] -> Clausula
clausulaUnion c1 c2 = noRepetidos (concatenar c1 ++ concatenar c2)

clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q  --si es conjunción divide las claúsulas
clausulas (Or p q) = [clausulaUnion (clausulas p) (clausulas q)] --si es disyunción une las claúsulas
clausulas p =[[p]]

-------

-- Ejercicio extra (Hasta 1 punto)
dpll2 :: Prop -> Interpretacion
dpll2 phi = dpll (clausulas (fnc phi))
