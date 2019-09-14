-- Informatica (1º del Grado en Matematicas) Grupo 1
-- Examen 5 de evaluacion alternativa (3 de mayo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerias auxiliares
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import I1M.Cola
import I1M.PolOperaciones
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número natural x es un cuadrado perfecto si x es
-- el cuadrado de algún natural. Por ejemplo, 36 es un cuadrado perfecto
-- y 12 no lo es.  
-- 
-- Un número entero positivo sera ordenado si sus dígitos
-- aparecen en orden creciente o bien en orden decreciente. Por
-- ejemplo, 11468 y 974000 son ordenados y 16832 no lo es. 
-- 
-- Definir la lista infinita
--    cuadradosOrdenados :: [Integer]
-- cuyos elementos son los cuadrados que son ordenados. Por ejemplo, 
--    Main> take 19 cuadradosOrdenados
--    [1,4,9,16,25,36,49,64,81,100,144,169,225,256,289,400,441,841,900]
-- ---------------------------------------------------------------------

cuadradosOrdenados :: [Integer]
cuadradosOrdenados = filter ordenado [i^2 | i <- [1..]]

ordenado :: Integer -> Bool
ordenado x = cs == ds || cs == reverse ds
  where cs = digitos x
        ds = sort cs

digitos :: Integer -> [Int]
digitos x = [read [i] | i <- show x]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcula el mayor cuadrado ordenado de 7 dígitos. 
-- ---------------------------------------------------------------------

-- El cálculo es
--   Main> last (takeWhile (<=10^7-1) cuadradosOrdenados)
--   9853321

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    posicionesPares :: Cola a -> Cola a
-- tal que (posicionesPares p) devuelve la cola obtenida tomando los
-- elementos que ocupan las posiciones pares en la cola p. Por ejemplo,
--    Main> c = (foldr inserta vacia [0..9])
--    Main> d = (foldr inserta vacia [0..10])
--    Main> c
--    C [9,8,7,6,5,4,3,2,1,0]
--    Main> colaPosPares c
--    C [9,7,5,3,1]
--    Main> d
--    C [10,9,8,7,6,5,4,3,2,1,0]
--    Main> colaPosPares d
--    C [10,8,6,4,2,0]
--    Main> colaPosPares (foldr inserta  vacia "salamanca")
--    C "anmls"
-- ---------------------------------------------------------------------

colaPosPares :: Cola a -> Cola a
colaPosPares p = aux p vacia
  where aux c r 
          | esVacia c  = r
          | esVacia rc = inserta pc r
          | otherwise  = aux (resto rc) (inserta pc r)
          where (pc,rc) = (primero c, resto c)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una matriz de enteros p se dira "de unos en línea" si
-- cumple que:
--    toda fila de p contiene, como maximo un 1;
--    o bien toda columna de p contiene, como maximo un 1.
-- 
--  Definir la función
--     unoEnlinea :: Matrix Int -> Bool
--  tal que (unoEnlinea p) se verifica si la matriz p es de unos en
--  linea según la definición anterior. Por ejemplo,
--     Main> unoEnlinea (fromLists [[0,0,1,0,0],[1,0,0,0,0],[0,0,0,0,1]])
--     True
--     Main> unoEnlinea (fromLists [[0,0,0,0],[0,1,0,0],[0,0,0,1],[0,1,1,0]])
--     False
--     Main> unoEnlinea (fromLists [[0,1],[2,0]])
--     True
-- ---------------------------------------------------------------------

unoEnlinea :: Array (Int,Int) Int -> Bool
unoEnlinea p =
  all tieneComoMaximoUnUno (filas ++ columnas) 
  where (_,(m,n)) = bounds p
        filas     = [[p!(i,j) | j <- [1..n]] | i <- [1..m]]
        columnas  = [[p!(i,j) | i <- [1..m]] | j <- [1..n]]

tieneComoMaximoUnUno :: [Int] -> Bool
tieneComoMaximoUnUno xs = length (filter (==1) xs) <= 1

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    corte :: (Eq a,Num a) => Int -> Polinomio a -> Polinomio a
-- tal que (corte k p) es el polinomio formado por los términos de p de
-- grado mayor o igual que k. Por ejemplo,  
--    Main> corte 3 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5 + -7*x^3
--    Main> corte 2 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5 + -7*x^3 + x^2
--    Main> corte 4 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5
-- ---------------------------------------------------------------------

corte :: (Eq a, Num a) => Int -> Polinomio a -> Polinomio a
corte k p
  | k < 0       = polCero
  | esPolCero p = polCero
  | k > n       = polCero
  | otherwise   = consPol n a (corte k (restoPol p))
  where n = grado p
        a = coefLider p

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Un polinomio de enteros se dirá impar si su término
-- independiente es impar y el resto de sus coeficientes (si los
-- hubiera) son pares. 
-- 
-- Definir la función
--    imparPol :: Integral a => Polinomio a -> Bool
-- tal que (imparPol p) se verifica si p es un polinomio impar de
-- acuerdo con la definicion anterior. Por ejemplo,
--    Main> imparPol (consPol 5 2 (consPol 3 6 (consPol 0 3 polCero)))
--    True
--    Main> imparPol (consPol 5 2 (consPol 3 6 (consPol 0 4 polCero)))
--    False
--    Main> imparPol (consPol 5 2 (consPol 3 1 (consPol 0 3 polCero)))
--    False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

imparPol :: Integral a => Polinomio a -> Bool
imparPol p = odd (coeficiente 0 p) &&
             all even [coeficiente k p | k <- [1..grado p]]

coeficiente :: Integral a => Int -> Polinomio a -> a
coeficiente k p
  | esPolCero p = 0
  | k > n       = 0
  | k == n      = coefLider p
  | otherwise   = coeficiente k (restoPol p)
  where n = grado p

-- 2ª solución
-- ===========

imparPol2 :: Integral a => Polinomio a -> Bool
imparPol2 p =
  all even [coeficiente k (sumaPol p polUnidad) | k <- [0..grado p]]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Un conjunto V de nodos de un grafo no dirigido g forma
-- un club si dos elementos cualesquiera de V tienen una arista en g que
-- los conecta. Por ejemplo, en el grafo:
--      4 ---- 5 
--      |      | \
--      |      |  1
--      |      | /
--      3 ---- 2   
-- el conjunto de vértices {1,2,5} forma un club y el conjunto {2,3,4,5}
-- no.
-- 
-- En Haskell se puede representar el grafo anterior por
--    g1 :: Grafo Int Int
--    g1 = creaGrafo ND
--                   (1,5) 
--                   [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0)]
-- 
-- Definir la función
--    esUnClub :: Grafo Int Int -> [Int] -> Bool
-- tal que (esUnClub g xs) se verifica si xs forma un club en g. Por
-- ejemplo, 
--    esUnClub g1 [1,2,5]   == True
--    esUnClub g1 [2,3,4,5] == False
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int
g1 = creaGrafo ND
               (1,5) 
               [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0)]

esUnClub :: Grafo Int Int -> [Int] -> Bool
esUnClub g xs = all (aristaEn g) [(x,y) | x <- ys, y <- ys, y < x]
    where ys = sort xs

-- ---------------------------------------------------------------------
-- Ejercicio 6. El siguiente triángulo se construye sabiendo
-- que se verifica la siguiente relación 
--    A(n,m)=(n-m)A(n-1,m-1) + (m+1)A(n-1,m).
-- Sus primeros términos son
--    1 
--    1 0                                                       
--    1 1   0                                               
--    1 4   1     0                                       
--    1 11  11    1     0                               
--    1 26  66    26    1      0                       
--    1 57  302   302   57     1     0               
--    1 120 1191  2416  1191   120   1     0       
--    1 247 4293  15619 15619  4293  247   1   0
--    1 502 14608 88234 156190 88234 14608 502 1 0
-- 
-- Definir las siguientes funciones:
--   num           :: Integer -> Integer -> Integer
--   filaTriangulo :: Integer -> [Integer]
--   triangulo     :: [[Integer]]
-- tales que
-- + (num n k) es el número A(n,k). Por ejemplo, 
--      num 8 3  == 15619
--      num 20 6 == 21598596303099900
-- + (filaTriangulo n) es la n-sima fila del triángulo. Por ejemplo, 
--      ghci> filaTriangulo 8
--      [1,247,4293,15619,15619,4293,247,1]
--      ghci> filaTriangulo 12
--      [1,4083,478271,10187685,66318474,162512286,162512286,66318474,
--       10187685,478271,4083,1]
-- + triangulo es la lista con las filas del triángulo
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

triangulo1 :: [[Integer]]
triangulo1 = iterate siguiente [1]

siguiente :: [Integer] -> [Integer]
siguiente xs = zipWith (+) us vs
  where n = genericLength xs
        us = [x*k | (x,k) <-zip (0:xs) [n+1,n..1]]
        vs = [x*k | (x,k) <-zip (xs++[0]) [1..n+1]]

-- Otra definición de siguiente es
siguiente' :: [Integer] -> [Integer]
siguiente' xs = zipWith (+) us vs
  where n = genericLength xs
        us = zipWith (*) (0:xs) [n+1,n..1]
        vs = zipWith (*) (xs++[0]) [1..n+1]

filaTriangulo1 :: Integer -> [Integer]
filaTriangulo1 n = triangulo1 `genericIndex` (n-1)

num1 :: Integer -> Integer -> Integer
num1 n k = (filaTriangulo1 n) `genericIndex` k

-- 2ª solución

num :: Integer -> Integer -> Integer
num n k = (matrizE n k) ! (n,k)

matrizE :: Integer -> Integer -> Array (Integer,Integer) Integer
matrizE n m = q
  where q = array ((0,0),(n,m)) [((i,j), f i j) | i <-[0..n],j<-[0..m]]
        f i 0 = 1
        f i j
          | i == j    = 0
          | otherwise = (i-j) * q!(i-1,j-1) + (j+1)* q!(i-1,j)

filaTriangulo :: Integer -> [Integer]
filaTriangulo n = map (num n) [0..n]

triangulo :: [[Integer]]
triangulo = map filaTriangulo [0..]
