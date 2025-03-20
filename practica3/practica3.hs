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
sonComplemento (Var p) (Not (Var q)) = p == q
sonComplemento (Not (Var p)) (Var q) = p == q
sonComplemento _ _ = False

--FUnción que elimina una literal de una claúsula
eliminar :: Literal -> Clausula ->Clausula
eliminar _ [] = []
eliminar l (x:xs) =
    if l == x
        then eliminar l xs
        else x : eliminar l xs
             
--Funcion que devuelve el complemento de dos literales en formato de tupla
complementoDos :: Literal -> Clausula -> (Literal, Literal)
complementoDos lt [] = (Cons False, Cons False)
complementoDos lt (x:xs) = if sonComplemento lt x == True
  then (lt, x)
  else complementoDos lt xs
  
--Función que ve si hay un complemento en una claúsula
complemento :: Clausula -> Clausula -> (Literal, Literal)
complemento [] cl2 = (Cons False, Cons False)
complemento (l1:xl1) cl2 =
  let (c1, c2) = complementoDos l1 cl2
  in if c1 == Cons False && c2 == Cons False
  then complemento xl1 cl2
  else (c1, c2)
  
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 =
    let (l1, l2) = complemento c1 c2 --BUscamos el complemento
    in if l1 == Cons True && l2 == Cons True
       then noRepetidos (c1 ++ c2)  -- Si no hay complemento, unimos
       else noRepetidos (eliminar l1 c1 ++ eliminar l2 c2)  -- Eliminamos complementos y devolver el resultado
            
-- 3.4 Algoritmo de saturación

-- Ejercicios

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente cl1 cl2 = if resolucion cl1 cl2 == noRepetidos (cl1 ++ cl2) then False else True 

-- Ejercicio 2

-- Funcion que dado un conjunto S, aplica resolucion sobre el primer elemento fijo de S con el resto de formulas
aplicar :: [Clausula] -> [Clausula]
aplicar [] = []
aplicar (x:xs) = [resolucion x k | k <- xs, hayResolvente x k == True] ++ aplicar xs

-- Funcion que verifica si hay una clausula vacia dentro de un conjunto S
hayVacia :: [Clausula] -> Bool
hayVacia [] = False
hayVacia (x:xs) = if x == [] then True else hayVacia xs

-- Funcion que recibe dos conjuntos Rn Rn+1 y devuelve
res :: [Clausula]->[Clausula]-> Bool
res cjt_anterior  cjt_actual =
  let res_actual = cjt_actual ++ aplicar cjt_actual in 
  if hayVacia res_actual == False then -- si no se encuentra la vacia.
	if cjt_anterior == cjt_actual then False -- No se pudo resolver o se cicla lo que pase primero por def.
	else res cjt_actual (cjt_actual ++ aplicar cjt_actual)
  else True -- regresa verdadero si se pudo resolver, es decir, la vacia fue encontrada.

saturacion :: Prop -> Bool
saturacion phi =
  let cjt0 = clausulas phi
      cjt1 = cjt0 ++ aplicar cjt0
  in if res cjt0 cjt1 == True then False else True
