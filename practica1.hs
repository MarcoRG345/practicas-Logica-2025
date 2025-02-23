
--funcion aux para obtener la longitud de una lista
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

-- define una funcion que dice si es --
-- palindromo.
isPal :: String -> Bool
isPal [] = True
isPal [a] = True
isPal (x:xs) = if x /= last xs then False
  else isPal (init xs) 
-- define una funcion que concatena dos listas
concat' :: [[a]] -> [a]
concat' []  =  []
concat' [[]] = []
concat' (x:xs) = (x ++ concat' xs)

-- define una funcion que regresa la n-esima fila
-- del triangulo de pascal
--funcion aux calcular el factorial de un numero
fact :: Int -> Int
fact 1 = 1
fact 0 = 1
fact n  = n * fact(n-1)

pascalN :: Int -> [Int]
pascalN 0 = [1]
pascalN n = [(fact n `div` ((fact k)*fact(n - k))) | k<-[0..n]]

-- define una funcion recibe una lista y regresala
-- en orden opuesto
agregaFinal :: a -> [a] -> [a]
agregaFinal x xs = xs ++ [x]


reversaFr :: [a] -> [a]
reversaFr [] = []
reversaFr [a] = [a]
reversaFr xs = foldr agregaFinal [] xs

-- Arboles
data OneTwoTree a = Vacio | NodeChild a (OneTwoTree a) | NodeBinary a (OneTwoTree a) (OneTwoTree a) 
suma :: OneTwoTree  Int -> Int
suma Vacio = 0
suma (NodeChild node child) = node + suma child
suma (NodeBinary node child1 child2 ) = node + (suma child1) + (suma child2)
sinCero :: OneTwoTree Int -> Bool
sinCero Vacio = False
sinCero (NodeChild node child) = if node == 0 then False
  else sinCero child
sinCero (NodeBinary node child1 child2) = if node == 0 then False
  else sinCero child1 && sinCero child2


