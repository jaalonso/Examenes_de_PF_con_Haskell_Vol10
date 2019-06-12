-- Informática (1º del Grado en Matemáticas, Grupos 2, 3 y 5)
-- 6º examen de evaluación continua (10 de junio de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Char
import Data.Array
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número de n dígitos es un número de Armstrong si es
-- igual a la suma de las n-ésimas potencias de sus dígitos. Por
-- ejemplo, 371, 8208 y 4210818 son números de Armstrong ya que 
--        371 = 3^3 + 7^3 + 1^3 y  
--       8208 = 8^4 + 2^4 + 0^4 + 8^4 
--    4210818 = 4^7 + 2^7 + 1^7 + 0^7 + 8^7 + 1^7 + 8^7
--
-- Definir las funciones
--    esArmstrong :: Integer -> Bool
--    armstrong :: [Integer]
-- tales que
-- + (esArmstrong x) se verifica si x es un número de Armstrong. Por
--   ejemplo, 
--      esArmstrong 371                                      ==  True
--      esArmstrong 8208                                     ==  True
--      esArmstrong 4210818                                  ==  True
--      esArmstrong 2015                                     ==  False
--      esArmstrong 115132219018763992565095597973971522401  ==  True
--      esArmstrong 115132219018763992565095597973971522402  ==  False
-- + armstrong es la lista cuyos elementos son los números de
--   Armstrong. Por ejemplo, 
--      λ> take 18 armstrong
--      [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727]
--
-- Comprobar con QuickCheck que los números mayores que 
-- 115132219018763992565095597973971522401 no son números de Armstrong.
-- ---------------------------------------------------------------------

esArmstrong :: Integer -> Bool
esArmstrong x = 
  x == sum [d^n | d <- digitos x]
  where n = length (show x)

-- (digitos x) es la lista de los dígitos de x. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos x = [read [d] | d <- show x]

armstrong :: [Integer]
armstrong = [n | n <- [1..], esArmstrong n]

-- La propiedad es
prop_Armstrong :: Integer -> Bool
prop_Armstrong n =
  not (esArmstrong (115132219018763992565095597973971522401 + abs n + 1))

-- La comprobación es
--    λ> quickCheck prop_Armstrong
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un número n es k-belga si la sucesión cuyo primer
-- elemento es k y cuyos elementos se obtienen sumando reiteradamente
-- los dígitos de n contiene a n.  
-- 
-- El 18 es 0-belga, porque a partir del 0 vamos a ir sumando
-- sucesivamente 1, 8, 1, 8, ... hasta llegar o sobrepasar el 18: 0, 1,
-- 9, 10, 18, ... Como se alcanza el 18, resulta que el 18 es 0-belga. 
-- 
-- El 19 no es 1-belga, porque a partir del 1 vamos a ir sumando
-- sucesivamente 1, 9, 1, 9, ... hasta llegar o sobrepasar el 18: 1, 2,
-- 11, 12, 21, ... Como no se alcanza el 19, resulta que el 19 no es
-- 1-belga. 
--
-- Definir la función 
--    esBelga :: Integer -> Integer -> Bool
-- tal que (esBelga k n)  se verifica si n es k-belga. Por ejemplo,
--    esBelga 0 18                              ==  True
--    esBelga 1 19                              ==  False
--    esBelga 0 2016                            ==  True
--    [x | x <- [0..30], esBelga 7 x]           ==  [7,10,11,21,27,29]
--    [x | x <- [0..30], esBelga 10 x]          ==  [10,11,20,21,22,24,26]
--    length [n | n <- [1..9000], esBelga 0 n]  ==  2857
--
-- Comprobar con QuickCheck que para todo número entero positivo n, si
-- k es el resto de n entre la suma de los dígitos de n, entonces n es
-- k-belga.
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

esBelga1 :: Integer -> Integer -> Bool
esBelga1 k n =
  n == head (dropWhile (<n) (scanl (+) k (cycle (digitos n))))

-- 2ª solución
-- ===========

esBelga2 :: Integer -> Integer -> Bool
esBelga2 k n =
  k <= n && n == head (dropWhile (<n) (scanl (+) (k + q * s) ds))
  where ds = digitos n
        s  = sum ds
        q  = (n - k) `div` s

-- Comparación de eficiencia
-- =========================

--    λ> length [n | n <- [1..9000], esBelga1 0 n]
--    2857
--    (2.95 secs, 1,115,026,728 bytes)
--    λ> length [n | n <- [1..9000], esBelga2 0 n]
--    2857
--    (0.10 secs, 24,804,480 bytes)

-- Equivalencia
-- ============

-- La propiedad es
prop_equivalencia :: Integer -> Integer -> Property
prop_equivalencia k n = 
  n > 0 ==> esBelga1 k n == esBelga2 k n

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Verificación de la propiedad
-- ============================

-- La propiedad es
prop_Belga :: Integer -> Property
prop_Belga n = 
  n > 0 ==> esBelga2 k n
  where k = n `mod` sum (digitos n)

-- La comprobación es
--    λ> quickCheck prop_Belga
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. El problema del número de emparejamiento de amigos
-- consiste en calcular el número de formas de emparejar n amigos
-- teniendo en cuenta que cada uno puede permanecer soltero o puede ser
-- emparejado con algún otro amigo y que cada amigo puede ser emparejado
-- sólo una vez. Por ejemplo, los 4 posibles emparejamientos de 3 amigos
-- son   
--    {1}, {2}, {3} 
--    {1}, {2, 3} 
--    {1, 2}, {3} 
--    {1, 3}, {2}
--
-- Definir, usando programación dinámica, la función
--    nEmparejamientos :: Integer -> Integer
-- tal que (nEmparejamientos n) es el número de formas de emparejar a
-- los n amigos. Por ejemplo,  
--    nEmparejamientos 2   ==  2
--    nEmparejamientos 3   ==  4
--    nEmparejamientos 4   ==  10
--    nEmparejamientos 10  ==  9496
--    nEmparejamientos 30  ==  606917269909048576
--    length (show (nEmparejamientos3 (10^4)))  ==  17872
-- ---------------------------------------------------------------------

nEmparejamientos :: Integer -> Integer
nEmparejamientos n = (vectorEmparejamientos n) ! n

-- (vectorEmparejamientos n) es el vector con índices de 0 a n tal que el valor
-- de la posición i es el número de formas de emparejar a i amigos. Por ejemplo,
--    λ> vectorEmparejamientos 7
--    array (0,7) [(0,1),(1,1),(2,2),(3,4),(4,10),(5,26),(6,76),(7,232)]
vectorEmparejamientos :: Integer -> Array Integer Integer
vectorEmparejamientos n = v where
  v = array (0,n) [(i,f i) | i <- [0..n]]
  f 0 = 1
  f 1 = 1
  f 2 = 2
  f k = v!(k-1) + (k-1)*v!(k-2)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se dice que a es un divisor propio maximal de un número
-- b si a es un divisor de b distinto de b y no existe ningún número c
-- tal que a < c < b, a es un divisor de c y c es un divisor de b. Por
-- ejemplo, 15 es un divisor propio maximal de 30, pero 5 no lo es.
--
-- El árbol de los divisores de un número x es el árbol que tiene como
-- raíz el número x y cada nodo tiene como hijos sus divisores propios
-- maximales. Por ejemplo, el árbol de divisores de 30 es
--
--            30
--            /|\
--           / | \
--          /  |  \
--         /   |   \
--        /    |    \
--       /     |     \
--      6     10     15
--     / \    / \    / \
--    2   3  2   5  3   5
--    |   |  |   |  |   |
--    1   1  1   1  1   1
--
-- Usando el tipo de dato
--    data Arbol = N Integer [Arbol]
--      deriving (Eq, Show)
-- el árbol anterior se representa por
--    N 30
--      [N 6
--         [N 2 [N 1 []],
--          N 3 [N 1 []]],
--       N 10
--         [N 2 [N 1 []],
--          N 5 [N 1 []]],
--       N 15
--         [N 3 [N 1 []],
--          N 5 [N 1 []]]]
--
-- Definir las funciones 
--    arbolDivisores             :: Integer -> Arbol
--    nOcurrenciasArbolDivisores :: Integer -> Integer -> Integer
-- tales que
-- + (arbolDivisores x) es el árbol de los divisores del número x. Por
--   ejemplo,  
--      λ> arbolDivisores 30
--      N 30 [N 6  [N 2 [N 1 []],N 3 [N 1 []]],
--            N 10 [N 2 [N 1 []],N 5 [N 1 []]],
--            N 15 [N 3 [N 1 []],N 5 [N 1 []]]]
-- + (nOcurrenciasArbolDivisores x y) es el número de veces que aparece
--   el número x en el árbol de los divisores del número y. Por ejemplo,  
--      nOcurrenciasArbolDivisores  3 30  ==  2
--      nOcurrenciasArbolDivisores  6 30  ==  1
--      nOcurrenciasArbolDivisores 30 30  ==  1
--      nOcurrenciasArbolDivisores  1 30  ==  6
--      nOcurrenciasArbolDivisores  9 30  ==  0
--      nOcurrenciasArbolDivisores  2 (product [1..10])  ==  360360
--      nOcurrenciasArbolDivisores  3 (product [1..10])  ==  180180
--      nOcurrenciasArbolDivisores  5 (product [1..10])  ==  90090
--      nOcurrenciasArbolDivisores  7 (product [1..10])  ==  45045
--      nOcurrenciasArbolDivisores  6 (product [1..10])  ==  102960
--      nOcurrenciasArbolDivisores 10 (product [1..10])  ==  51480
--      nOcurrenciasArbolDivisores 14 (product [1..10])  ==  25740
-- ------------------------------------------------------------------------

data Arbol = N Integer [Arbol]
  deriving (Eq, Show)

-- Definición de arbolDivisores
-- ============================

arbolDivisores :: Integer -> Arbol
arbolDivisores x =
  N x (map arbolDivisores (divisoresPropiosMaximales x))

-- (divisoresPropiosMaximales x) es la lista de los divisores propios
-- maximales de x. Por ejemplo,
--    divisoresPropiosMaximales 30   ==  [6,10,15]
--    divisoresPropiosMaximales 420  ==  [60,84,140,210]
--    divisoresPropiosMaximales 7    ==  [1]
divisoresPropiosMaximales :: Integer -> [Integer]
divisoresPropiosMaximales x =
  reverse [x `div` y | y <- nub (primeFactors x)]

-- Definición de nOcurrenciasArbolDivisores
-- ========================================
  
nOcurrenciasArbolDivisores :: Integer -> Integer -> Integer
nOcurrenciasArbolDivisores x y =
  nOcurrencias x (arbolDivisores y)

-- (nOcurrencias x a) es el número de veces que aprece x en el árbol
-- a. Por ejemplo,
--    nOcurrencias 3 (arbolDivisores 30)  ==  2
nOcurrencias :: Integer -> Arbol -> Integer
nOcurrencias x (N y [])
  | x == y    = 1
  | otherwise = 0
nOcurrencias x (N y zs)
  | x == y    = 1 + sum [nOcurrencias x z | z <- zs]
  | otherwise = sum [nOcurrencias x z | z <- zs]
