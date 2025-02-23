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
  show (Not p) = "" ++ show p
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

-- Ejercicio 1
variables :: Prop -> [String]
variables = undefined

-- Ejercicio 2
conjPotencia :: [a] -> [[a]]
conjPotencia = undefined

-- Ejercicio 3
interpretacion :: Prop -> Estado -> Bool
interpretacion = undefined

-- Ejercicio 4
estadosPosibles :: Prop -> [Estado]
estadosPosibles = undefined

-- Ejercicio 5
modelos :: Prop -> [Estado]
modelos = undefined

-- Ejercicio 6
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes = undefined

-- Ejercicio 7
tautologia :: Prop -> Bool
tautologia = undefined

-- Ejercicio 8
contradiccion :: Prop -> Bool
contradiccion = undefined

-- Ejercicio 9
contingencia :: Prop -> Bool 
contingencia = undefined

-- Ejercicio 10
esModelo :: Estado -> Prop -> Bool
esModelo = undefined

-- Ejercicio 11
esSatisfacible :: Prop -> Bool
esSatisfacible = undefined

