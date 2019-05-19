-- Informática (1º del Grado en Matemáticas), Grupo 5
-- 5º examen de evaluación continua (16 de mayo de 2019)
-- ------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Maybe
import Data.Array
import I1M.Pol

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--     maxConUno :: [Integer] -> Maybe Integer
-- tal que (maxConUno xs) es (Just x) si x es el mayor elemento de la
-- lista xs que empieza por uno, o bien Nothing si xs no contiene ningún
-- elemento que empiece por uno. Por ejemplo, 
--    maxConUno [23,1134,107,56,1117] == Just 1134
--    maxConUno [23,2311,99]          == Nothing
-- ---------------------------------------------------------------------

maxConUno :: [Integer] -> Maybe Integer
maxConUno xs
  | null ys   = Nothing
  | otherwise = Just (maximum ys)
  where ys = filter (\x -> head (show x) == '1') xs

-- ------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios mediante el tipo
-- de dato
--    data Arbol a = H a | N a (Arbol a) (Arbol a)
--                   deriving Show                        
-- 
-- Definir la función
--    todasOninguno :: (a -> Bool) -> Arbol a -> Bool
-- tal que (todasOninguno p t) se verifica si todas las hojas del árbol t
-- satisfacen la propiedad p o ningún nodo interno del árbol t satisface
-- p. Por ejemplo, 
--    todasOninguno even (N 1 (H 4) (N 2 (H 0) (H 8)))  ==  True
--    todasOninguno (>0) (N 1 (H 4) (N 2 (H 0) (H 8)))  ==  False
--    todasOninguno (>4) (N 1 (H 4) (N 2 (H 0) (H 8)))  ==  True
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a)
               deriving Show                        

todasOninguno :: (a -> Bool) ->  Arbol a -> Bool
todasOninguno p a = todas p a || ninguno p a


-- (todas p t) se verifica si todas las hojas del árbol t satisfacen la
-- propiedad p. Por ejemplo, 
--    todas even (N 1 (H 4) (N 2 (H 0) (H 8)))  ==  True
--    todas even (N 1 (H 4) (N 2 (H 1) (H 8)))  ==  False
todas :: (a -> Bool) ->  Arbol a -> Bool
todas p (H x)     = p x
todas p (N x i d) = todas p i && todas p d

-- (ninguno p t) se verifica si ningún nodo interno del árbol t
-- satisface p. Por ejemplo,
--    ninguno (>2) (N 1 (H 4) (N 2 (H 1) (H 8)))  ==  True
--    ninguno (>0) (N 1 (H 4) (N 2 (H 1) (H 8)))  ==  False
ninguno :: (a -> Bool) ->  Arbol a -> Bool
ninguno p (H x)     = True
ninguno p (N x i d) = (not . p) x && ninguno p i && ninguno p d

-- ---------------------------------------------------------------------
-- Ejercicio 3. Define la función
--    calculadora :: IO ()
-- que represente mediante entrada y salida la funcionalidad de una
-- calculadora con las siguientes operaciones: suma (+), resta (-),
-- división (/), multiplicación (*) y terminar (q). Un ejemplo de
-- interacción es el siguiente:
--  λ> calculadora
--    Introduzca una operación (+,-,/,*,q): sumar
--    Operación incorrecta
--    Introduzca una operación (+,-,/,*,q): +
--    Introduzca el primer operando: 2.3
--    Introduzca el segundo operando: 2.7
--    El resultado es 5.0
--    Introduzca una operación (+,-,/,*,q): q
--    Fin.
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

calculadora :: IO ()
calculadora = do
  putStr "Introduzca una operación (+,-,/,*,^,q): "
  os <- getLine
  if os == "q" then
    putStrLn "Fin"
  else if os `notElem` ["+","-","/","*","^"] then do
    putStrLn "Operación incorrecta"
    calculadora
  else do
    putStr "Introduzca el primer operando: "
    os1 <- getLine
    let op1 = read os1 :: Float
    putStr "Introduzca el segundo operando: "
    os2 <- getLine
    let op2 = read os2 :: Float
    let res = operacion os op1 op2
    putStrLn ("El resultado es " ++ show res)
    calculadora
    
operacion :: String -> Float -> Float -> Float
operacion os op1 op2
   | os == "+" = op1 + op2
   | os == "-" = op1 - op2
   | os == "/" = op1 / op2
   | os == "*" = op1 * op2
   
-- 2ª solución
-- ===========

calculadora2 :: IO ()
calculadora2 = do
  putStr "Introduzca una operación (+,-,/,*,^,q): "
  os <- getLine
  if os == "q" then
    putStrLn "Fin"
  else if os `notElem` ["+","-","/","*","^"] then do
    putStrLn "Operación incorrecta"
    calculadora
  else do
    putStr "Introduzca el primer operando: "
    op1 <- read <$> getLine :: IO Float
    putStr "Introduzca el segundo operando: "
    op2 <- read <$> getLine :: IO Float
    putStrLn ("El resultado es " ++ show (operacion2 os op1 op2))
    calculadora
    
operacion2 :: String -> Float -> Float -> Float
operacion2 os =
  case os of
    "+" -> (+)
    "-" -> (-)
    "/" -> (/)
    "*" -> (*)

-- -------------------------------------------------------------------
-- Ejercicio 4.  Representamos los polinomios mediante el TAD de los
-- Polinomios (I1M.Pol).
--
-- Definir la función
--   combina :: (Ord a,Num a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (combina p q) es el polinomio formado a partir de p y q como
-- sigue: 
--    1) Si para un grado m hay términos de grado m en ambos polinomios
--       p y q, entonces en la combinación se elige el coeficiente
--       menor en valor absoluto (o, en caso de coincidencia en valor
--       absoluto, el coeficiente positivo);
--    2) Si para un grado m solo hay términos de grado m en uno de los
--       dos polinomios, dicho término aparece tal cual en la
--       combinación. 
-- Por ejemplo, si pol1 y pol2 son, respectivamente, los siguientes
-- polinomios
--    3*x^5 + -2*x^4 + 5*x + 11
--    7*x^4 + x^2 + -5*x + -3
-- entonces
--    λ> combina pol1 pol2
--    3*x^5 + -2*x^4 + x^2 + 5*x + -3
-- ---------------------------------------------------------------------

pol1, pol2 :: Polinomio Int
pol1 = consPol 5 3 (consPol 4 (-2) (consPol 1 5 (consPol 0 11 polCero)))
pol2 = consPol 4 7 (consPol 2 1 (consPol 1 (-5) (consPol 0 (-3) polCero)))

combina :: (Ord a,Num a) => Polinomio a -> Polinomio a -> Polinomio a
combina p1 p2
  | esPolCero p1 = p2
  | esPolCero p2 = p1
  | n1 == n2     = consPol n1 (combinaCoef b1 b2) (combina r1 r2)
  | n1 > n2      = consPol n1 b1 (combina r1 p2)
  | n1 < n2      = consPol n2 b2 (combina p1 r2)
  where n1 = grado p1
        n2 = grado p2
        b1 = coefLider p1
        b2 = coefLider p2
        r1 = restoPol p1
        r2 = restoPol p2
        combinaCoef a1 a2 | abs a1 == abs a2 = max a1 a2
                          | abs a1 <  abs a2 = a1
                          | abs a1 >  abs a2 = a2
                          
-- ---------------------------------------------------------------------
-- Ejercicio 5. Representamos la matrices mediante el tipo de dato
--    type Matriz a = Array (Int,Int) a
--
-- Una matriz de ceros y unos se puede representar mediante una lista
-- de triples como sigue:
--   el triple (i,v,ps) aparece en la representación si v es el valor
--   minoritario de la fila i-ésima de la matriz (en caso de empate,
--   eligiremos el cero como valor minoritario); y ps es la lista
--   de las posiciones de v en la fila i-ésima.
-- Por ejemplo, la matriz
--      (1 0 1 0)
--      (0 0 1 0)
--      (1 1 1 1)
-- se representa por [(1,0,[2,4]),(2,1,[3]),(3,0,[])]
-- 
-- Definir la función
--    triples :: Matriz Int -> [(Int,Int,[Int])]
-- tal que (triples p) es la lista de los triples correspondientes a la
-- matriz p según lo arriba descrito. Por ejemplo, 
--    λ> triples (listArray ((1,1),(3,4)) [1,0,1,0,0,0,0,1,1,1,1,1])
--    [(1,1,[1,3]),(2,1,[4]),(3,0,[])]
--    λ> triples (listArray ((1,1),(4,4)) [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1])
--    [(1,1,[1]),(2,1,[2]),(3,1,[3]),(4,1,[4])]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

triples :: Matriz Int -> [(Int,Int,[Int])]
triples p = [(i,minelem i, minpos i) | i <- [1..m]]
  where
    (_,(m,n)) = bounds p
    minelem i | 2 * sum [p!(i,j) | j <- [1..n]] > n = 0
              | otherwise                            = 1
    minpos i  = [j | j <- [1..n], p!(i,j) == minelem i]
       
