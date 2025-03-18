module Practica03 where

-- Práctica 03: Resolución Binaria

-- 3.1 Lógica Proposicional

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


-- 3.2 Formas Normales

-- Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons a) = Cons a
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not p) = Not (fnn p)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

-- Ejercicio 2
-- Función para distribuir y pasarla a su forma normal conjuntiva
distribuir :: Prop -> Prop -> Prop
distribuir (And p q) r = And (distribuir p r) (distribuir q r)
distribuir p (And q r) = And (distribuir p q) (distribuir p r)
distribuir p q = Or p q

fnc :: Prop -> Prop
fnc prop = fncdos (fnn prop) --Primero convertimos a fnn y ya luego a fnc
  where
    fncdos :: Prop -> Prop
    fncdos (And p q) = And (fncdos p) (fncdos q) --COnjunción recursiva
    fncdos (Or p q)  = distribuir (fncdos p) (fncdos q) --Dsitribuimos or sobre and
    fncdos p = p 

-- 3.3 Resolución Binaria

-- Sinónimo Literal
type Literal = Prop

-- Sinónimo Cláusula
type Clausula = [Literal]


-- Ejercicios

-- Ejercicio 1
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

-- Ejercicio 2
--Función para ver si dos claúsulas son complemento
sonComplemento :: Literal -> Literal -> Bool
sonComplemento (Not p) q = p == q
sonComplemento p (Not q) = p == q
sonComplemento _ _ = False

--FUnción que elimina una literal de una claúsula
eliminar :: Literal -> Clausula ->Clausula
eliminar _ [] = []
eliminar l (x:xs) =
    if l == x
        then eliminar l xs
        else x : eliminar l xs

--Función que ve si hay un complemento en una claúsula
hayComplemento :: Clausula -> Clausula -> (Literal, Literal)
hayComplemento [] _ = (Cons True, Cons True)  --Caso base, no hay
hayComplemento (l1:ls1) c2 =
    let complemento = hayComplementoEnC2 l1 c2
    in if complemento /= (Cons True, Cons True) then complemento else hayComplemento ls1 c2
  where
    --Lo busca en la segunda claúsula
    hayComplementoEnC2 _ [] = (Cons True, Cons True) --Caso base, no hay
    hayComplementoEnC2 l1 (l2:ls2) =
        if sonComplemento l1 l2 then (l1, l2) else hayComplementoEnC2 l1 ls2

resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
    let (l1, l2) = hayComplemento c1 c2 --BUscamos el complemento
    in if l1 == Cons True && l2 == Cons True
       then noRepetidos (c1 ++ c2)  -- Si no hay complemento, unimos
       else noRepetidos (eliminar l1 c1 ++ eliminar l2 c2)  -- Eliminamos complementos y devolver el resultado
-- 3.4 Algoritmo de saturación

-- Ejercicios

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion = undefined
