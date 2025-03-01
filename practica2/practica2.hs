module Practica02 where

-- Práctica 02: Conceptos Semánticos

-- 3.1 Sintaxis de la lógica proposicion

-- Tipo de datos Prop
data Prop =
  Var String |
  Cons Bool |
  Not Prop |
  And Prop Prop |
  Or Prop Prop |
  Impl Prop Prop |
  Syss Prop Prop
  deriving (Eq)

-- Imprimir tipo de datos Prop
instance Show Prop where
  show (Cons True)  = "Verdadero"
  show (Cons False) = "Falso"
  show (Var p) = p
  show (Not p) = "¬" ++ show p
  show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++show q ++ ")"

-- Variables proposicionales
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Estado
type Estado  = [String]

-- 3.2 Ejercicios

--Funcion auxiliar dada una lista regresa los no repetidos
noRepetidos :: [String] -> [String]
noRepetidos [] = []
noRepetidos (x:xs) = x:noRepetidos [k | k <- xs, k /= x]

-- Ejercicio 1
variables :: Prop -> [String]
variables (Var p) = [p]
variables (Not p) =  noRepetidos (variables p)
variables (And p q) = noRepetidos (variables p ++  variables q)
variables (Or p q) =  noRepetidos (variables p ++ variables q)
variables (Impl p q) = noRepetidos (variables p ++ variables q)
variables (Syss p q) = noRepetidos (variables p ++ variables q)

-- Ejercicio 2
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

-- Ejercicio 3
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _ = b
interpretacion (Var p) [] = False
interpretacion (Var p) (x:xs) = if p == x then True else interpretacion (Var p) xs
interpretacion (Not p) xs =  if interpretacion p xs == False then True else False
interpretacion (And p q) xs = if interpretacion p xs == True && interpretacion q xs == True then True else False
interpretacion (Or p q) xs = if interpretacion p xs == False && interpretacion q xs == False then False else True
interpretacion (Impl p q) xs = if interpretacion p xs == True && interpretacion q xs== False then False else True
interpretacion (Syss p q) xs = if interpretacion p xs == interpretacion q xs then True else False



-- Ejercicio 4
estadosPosibles :: Prop -> [Estado]
estadosPosibles (Var p) = conjPotencia (variables (Var p))
estadosPosibles (Not p) = conjPotencia (variables (Not p))
estadosPosibles (And p q) = conjPotencia (variables (And p q))
estadosPosibles (Or p q) = conjPotencia (variables (Or p q))
estadosPosibles (Impl p q) = conjPotencia (variables (Impl p q))
estadosPosibles (Syss p q) = conjPotencia (variables (Syss p q))

-- Ejercicio 5
modelos :: Prop -> [Estado]
modelos formula = [estado | estado <- estadosPosibles formula, interpretacion formula estado]

-- Ejercicio 6
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes formula1 formula2 =
  let
    union :: [String] -> [String] -> [String]
    union xs ys = noRepetidos (xs ++ ys)
    variablesF1 = variables formula1
    variablesF2 = variables formula2
    variablesTodas = union variablesF1 variablesF2
  in
    all (\estado -> interpretacion formula1 estado == interpretacion formula2 estado)
        (conjPotencia variablesTodas)

-- Ejercicio 7
tautologia :: Prop -> Bool
tautologia formula = all (interpretacion formula) (estadosPosibles formula)

-- Ejercicio 8
contradiccion :: Prop -> Bool
contradiccion formula = all (not . interpretacion formula) (estadosPosibles formula)

-- Ejercicio 9
contingencia :: Prop -> Bool
contingencia formula = not (tautologia formula) && not (contradiccion formula)

-- Ejercicio 10
esModelo :: Estado -> Prop -> Bool
esModelo estado formula = interpretacion formula estado

-- Ejercicio 11
esSatisfacible :: Prop -> Bool
esSatisfacible formula = any (interpretacion formula) (estadosPosibles formula)

