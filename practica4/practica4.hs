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
conflict (_, [x]) = False
conflict (_, (x:xs)) = if x == [] then True else conflict ([] , xs)

-- Éxito
success :: Estado -> Bool
success (xs, []) = True
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

-- Funcion aux que dado una literal nos indica si esta presente en una clausula
contiene :: (String, Bool) -> Clausula -> Bool
contiene _ [] = False -- Caso base: No se encontro coincidencia, por lo que no se encuentra la literal
contiene (p, True) (x:xs) = if (Var p) ==  x then True else contiene (p, True) xs -- Caso rec: con interp verdadera verifica que sea igual a la literal si no sigue buscando.
contiene (p, False) (x:xs) = if (Not (Var p)) == x then True else contiene (p, False) xs -- Caso rec: con interp falsa verifica que sea igual a la literal si no sigue buscando.

-- Eliminación 
elim :: Estado -> Estado
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


-- 3.3 Implementación del algoritmo DPLL (Parte 2)

-- Heurística
heuristicsLiteral :: [Clausula] -> Literal 
heuristicsLiteral = undefined 

-- Función prinicipal
dpll :: [Clausula] -> Interpretacion
dpll = undefined 



-- Ejercicio extra (Hasta 1 punto)
dpll2 :: Prop -> Interpretacion
dpll2 = undefined 
