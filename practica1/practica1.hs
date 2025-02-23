--4.1 Tipos de Datos Algebraicos
-- Ejercicio 1): Shape
-- Definimos las figuras y usamos deriving show y eq para imprimir y comparar valores
data Shape = Circle Float
           | Square Float
           | Rectangle Float Float
           | Triangle Float
           | Trapeze Float Float Float
           deriving (Show, Eq)

-- 1) Funciones para calcular el área de cada figura 
area :: Shape -> Float
area (Circle r)
    | r <= 0 = error "El radio debe ser positivo"
    | otherwise = pi * r * r
area (Square l)
    | l <= 0 = error "El lado debe ser positivo"
    | otherwise = l * l
area (Rectangle b h)
    | b <= 0 || h <= 0 = error "La base y altura deben ser positivos"
    | otherwise = b * h
area (Triangle l)
    | l <= 0 = error "El lado debe ser positivo"
    | otherwise = (sqrt 3 / 4) * l * l
area (Trapeze b1 b2 h)
    | b1 <= 0 || b2 <= 0 || h <= 0 = error "Las bases y la altura deben ser positivas"
    | otherwise = ((b1 + b2) * h) / 2

-- 1) Funciones para calcular el perímetro de cada figura
perimeter :: Shape -> Float
perimeter (Circle r)
    | r <= 0 = error "El radio debe ser positivo"
    | otherwise = 2 * pi * r
perimeter (Square l)
    | l <= 0 = error "El lado debe ser positivo"
    | otherwise = 4 * l
perimeter (Rectangle b h)
    | b <= 0 || h <= 0 = error "La base y altura deben ser positivos"
    | otherwise = 2 * (b + h)
perimeter (Triangle l)
    | l <= 0 = error "El lado debe ser positivo"
    | otherwise = 3 * l
perimeter (Trapeze b1 b2 h)
    | b1 <= 0 || b2 <= 0 || h <= 0 = error "Las bases y la altura deben ser positivas"
    | otherwise = b1 + b2 + 2 * sqrt (((b1 - b2) / 2) ^ 2 + h ^ 2)

-- Comparamos las áreas
instance Ord Shape where
    compare x y = compare (area x) (area y)

-- Ejercicio 2): Point
-- Definimos point como una par de coordenadas
type Point = (Float, Float)

-- 2) Función para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- 2) Función para calcular la distancia del punto al origen
fromO :: Point -> Float
fromO p = distance p (0, 0)

-- Ejercicio 3: Haskellium's
-- Definimos a los haskelliums habitantes de la Isla Funcional
data Haskellium = Haskellium {
    name :: String,
    lastName1 :: String,
    lastName2 :: String,
    location :: Point,
    houseShape :: Shape
} deriving (Show)

-- 3) Función auxiliar para eliminar espacios innecesarios
removeSpace :: String -> String
removeSpace = unwords . words

-- 3) Función auixilar que válida si el nombre/apellido no tiene espacios
validName :: String -> Bool
validName n = not (null (removeSpace n))
-- 3) Función que crea a un hijo de dos haskelliums
son :: Haskellium -> Haskellium -> String -> Haskellium
son parent1 parent2 childName
    | not (validName (name parent1)) || not (validName (name parent2)) = error "Los padres deben tener nombres válidos"
    | not (validName (lastName1 parent1)) || not (validName (lastName1 parent2)) = error "Los padres deben tener apellidos válidos"
    | not (validName childName) = error "El hijo debe tener un nombre válido"
    | otherwise = Haskellium {
        name = removeSpace childName,  -- Se guarda el nombre sin espacios extra
        lastName1 = removeSpace (lastName1 parent1),
        lastName2 = removeSpace (lastName1 parent2),
        location = location parent1,
        houseShape = houseShape parent1
    }

-- 3) Función para calcular el costo de construir una casa
houseCost :: Haskellium -> Float
houseCost h = wallArea + roofArea
  where
    wallHeight = 2.5 -- altura de las paredes
    wallArea = perimeter (houseShape h) * wallHeight -- área lateral de las paredes
    roofArea = area (houseShape h) -- techo

-- 3) Función para calcular el tiempo de viaje a la plaza según distancia y transporte
timeToWork :: Haskellium -> Float
timeToWork h
    | distanceToPlaza < 0 = error "La distancia no puede ser negativa"
    | distanceToPlaza <= 300 = distanceToPlaza / 30 -- bicicleta (30 u/t)
    | otherwise = distanceToPlaza / 70 --moto (70 u/t)
  where
    plazaLocation = (0, 0) --sup. está en el origen
    distanceToPlaza = distance (location h) plazaLocation

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
sinCero Vacio = True
sinCero (NodeChild node child) = if node == 0 then False
  else sinCero child
sinCero (NodeBinary node child1 child2) = if node == 0 then False
  else sinCero child1 && sinCero child2
  

