-- Informática (1º del Grado en Matemáticas, Grupos 1 y 4)
-- 6º examen de evaluación continua (10 de junio de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Char 
import Data.Matrix
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    transformada :: [a] -> [a]
-- tal que (transformada xs) es la lista obtenida repitiendo cada
-- elemento tantas veces como indica su posición en la lista. Por
-- ejemplo, 
--    transformada [7,2,5] == [7,2,2,5,5,5]
--    transformada "eco"   == "eccooo"
-- 
-- Comprobar con QuickCheck si la longitud de la transformada de una
-- lista de n elementos es ((n+1)^3 - n^3 - 1)/6.
-- ---------------------------------------------------------------------

transformada :: [a] -> [a]
transformada xs = concat [replicate n x | (n,x) <- zip [1..] xs]

-- La propiedad es
prop_transformada :: [Int] -> Bool
prop_transformada xs =
  length (transformada xs) == ((n+1)^3 - n^3 - 1) `div` 6
  where n = length xs

-- La comprobación es
--    ghci> quickCheck prop_transformada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. La sucesión de Loomis generada por un número entero
-- positivo x es la sucesión cuyos términos se definen por 
-- + f(0) es x
-- + f(n) es la suma de f(n-1) y el producto de los dígitos no nulos de
--   f(n-1) 
-- Los primeros términos de las primeras sucesiones de Loomis son
-- + Generada por 1: 1,2,4,8,16,22,26,38,62,74,102,104,108,116,122,...
-- + Generada por 2: 2,4,8,16,22,26,38,62,74,102,104,108,116,122,126,...
-- + Generada por 3: 3,6,12,14,18,26,38,62,74,102,104,108,116,122,126,...
-- + Generada por 4: 4,8,16,22,26,38,62,74,102,104,108,116,122,126,138,...
-- + Generada por 5: 5,10,11,12,14,18,26,38,62,74,102,104,108,116,122,...
--
-- Se observa que a partir de un término todas coinciden con la
-- generada por 1. Dicho término se llama el punto de convergencia. Por
-- ejemplo,
-- + la generada por 2 converge a 2 
-- + la generada por 3 converge a 26
-- + la generada por 4 converge a 4
-- + la generada por 5 converge a 26
--
-- Definir las siguientes funciones
--    sucLoomis           :: Integer -> [Integer]
--    convergencia        :: Integer -> Integer
-- tales que
-- + (sucLoomis x) es la sucesión de Loomis generada por x. Por ejemplo,
--      λ> take 15 (sucLoomis 1)
--      [1,2,4,8,16,22,26,38,62,74,102,104,108,116,122]
--      λ> take 15 (sucLoomis 2)
--      [2,4,8,16,22,26,38,62,74,102,104,108,116,122,126]
--      λ> take 15 (sucLoomis 3)
--      [3,6,12,14,18,26,38,62,74,102,104,108,116,122,126]
--      λ> take 15 (sucLoomis 4)
--      [4,8,16,22,26,38,62,74,102,104,108,116,122,126,138]
--      λ> take 15 (sucLoomis 5)
--      [5,10,11,12,14,18,26,38,62,74,102,104,108,116,122]
--      λ> take 15 (sucLoomis 20)
--      [20,22,26,38,62,74,102,104,108,116,122,126,138,162,174]
--      λ> take 15 (sucLoomis 100)
--      [100,101,102,104,108,116,122,126,138,162,174,202,206,218,234]
--      λ> sucLoomis 1 !! (2*10^5)
--      235180736652
-- + (convergencia x) es el término de convergencia de la sucesión de
--   Loomis generada por x con la generada por 1. Por ejemplo, 
--      convergencia  2      ==  2
--      convergencia  3      ==  26
--      convergencia  4      ==  4
--      convergencia 17      ==  38
--      convergencia 19      ==  102
--      convergencia 43      ==  162
--      convergencia 27      ==  202
--      convergencia 58      ==  474
--      convergencia 63      ==  150056
--      convergencia 81      ==  150056
--      convergencia 89      ==  150056
--      convergencia (10^12) ==  1000101125092
-- ---------------------------------------------------------------------

-- 1ª definición de sucLoomis
-- ==========================

sucLoomis :: Integer -> [Integer]
sucLoomis x = map (loomis x) [0..]

loomis :: Integer -> Integer -> Integer
loomis x 0 = x
loomis x n = y + productoDigitosNoNulos y
  where y = loomis x (n-1)

productoDigitosNoNulos :: Integer -> Integer
productoDigitosNoNulos = product . digitosNoNulos

digitosNoNulos :: Integer -> [Integer]
digitosNoNulos x =
  [read [c] | c <- show x, c /= '0']

-- 2ª definición de sucLoomis
-- ==========================

sucLoomis2 :: Integer -> [Integer]
sucLoomis2 = iterate siguienteLoomis 

siguienteLoomis :: Integer -> Integer
siguienteLoomis y = y + productoDigitosNoNulos y

-- Comparación de eficiencia
-- =========================

--    λ> sucLoomis 1 !! 30000
--    6571272766
--    (2.45 secs, 987,955,944 bytes)
--    λ> sucLoomis2 1 !! 30000
--    6571272766
--    (2.26 secs, 979,543,328 bytes)

-- 1ª definición de convergencia
-- =============================

convergencia1 :: Integer -> Integer
convergencia1 x =
  head (dropWhile noEnSucLoomisDe1 (sucLoomis x))

noEnSucLoomisDe1 :: Integer -> Bool
noEnSucLoomisDe1 x = not (pertenece x sucLoomisDe1)

sucLoomisDe1 :: [Integer]
sucLoomisDe1 = sucLoomis 1

pertenece :: Integer -> [Integer] -> Bool
pertenece x ys =
  x == head (dropWhile (<x) ys)

-- 2ª definición de convergencia
-- =============================

convergencia2 :: Integer -> Integer
convergencia2 = aux (sucLoomis2 1) . sucLoomis2
  where aux as@(x:xs) bs@(y:ys) | x == y    = x
                                | x < y     = aux xs bs
                                | otherwise = aux as ys

-- 3ª definición de convergencia
-- =============================

convergencia3 :: Integer -> Integer
convergencia3 x = perteneceA (sucLoomis2 x) 1
  where perteneceA (y:ys) n | y == c    = y
                            | otherwise = perteneceA ys c
          where c = head $ dropWhile (< y) $ sucLoomis2 n

-- Comparación de eficiencia
-- =========================

--    λ> convergencia1 (10^4)
--    150056
--    (2.94 secs, 1,260,809,808 bytes)
--    λ> convergencia2 (10^4)
--    150056
--    (0.03 secs, 700,240 bytes)
--    λ> convergencia3 (10^4)
--    150056
--    (0.02 secs, 1,119,648 bytes)
--    
--    λ> convergencia2 (10^12)
--    1000101125092
--    (1.81 secs, 714,901,080 bytes)
--    λ> convergencia3 (10^12)
--    1000101125092
--    (1.82 secs, 941,053,328 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los caminos desde el extremo superior izquierdo
-- (posición (1,1)) hasta el extremo inferior derecho (posición (3,4))
-- en la matriz  
--    (  1  6 11  2 )
--    (  7 12  3  8 )
--    (  3  8  4  9 )
-- moviéndose en cada paso una casilla hacia abajo o hacia la derecha,
-- son los siguientes:
--    1, 7,  3, 8, 4, 9
--    1, 7, 12, 8, 4, 9
--    1, 7, 12, 3, 4, 9
--    1, 7, 12, 3, 8, 9
--    1, 6, 12, 8, 4, 9
--    1, 6, 12, 3, 4, 9
--    1, 6, 12, 3, 8, 9
--    1, 6, 11, 3, 4, 9
--    1, 6, 11, 3, 8, 9
--    1, 6, 11, 2, 8, 9
-- Las sumas de los caminos son 32, 41, 36, 40, 40, 35, 39, 34, 38 y 37,
-- respectivamente. El camino de máxima suma es el segundo (1, 7, 12, 8,
-- 4, 9) que tiene una suma de 41.
--
-- Definir, usando programación dinámica, la función
--    caminoMaxSuma :: Matrix Int -> [Int]
-- tal que (caminoMaxSuma m) es un camino de máxima suma en la matriz m
-- desde el extremo superior izquierdo hasta el extremo inferior
-- derecho, moviéndose en cada paso una casilla hacia abajo o hacia la 
-- derecha. Por ejemplo,
--    λ> caminoMaxSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    [1,7,12,8,4,9]
--    λ> sum (caminoMaxSuma (fromList 400 500 [1..]))
--    139576149
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

caminoMaxSuma1 :: Matrix Int -> [Int]
caminoMaxSuma1 m =
  head [c | c <- cs, sum c == k] 
  where cs = caminos1 m
        k  = maximum (map sum cs)

caminos1 :: Matrix Int -> [[Int]]
caminos1 m =
  map reverse (caminos1Aux m (nf,nc))
  where nf = nrows m
        nc = ncols m

-- (caminos1Aux p x) es la lista de los caminos invertidos en la matriz p
-- desde la posición (1,1) hasta la posición x. Por ejemplo,
caminos1Aux :: Matrix Int -> (Int,Int) -> [[Int]]
caminos1Aux m (1,1) = [[m!(1,1)]]
caminos1Aux m (1,j) = [[m!(1,k) | k <- [j,j-1..1]]]
caminos1Aux m (i,1) = [[m!(k,1) | k <- [i,i-1..1]]]
caminos1Aux m (i,j) = [m!(i,j) : xs
                      | xs <- caminos1Aux m (i,j-1) ++
                              caminos1Aux m (i-1,j)]

-- 2ª definición
-- =============

caminoMaxSuma2 :: Matrix Int -> [Int]
caminoMaxSuma2 m =
  head [c | c <- cs, sum c == k] 
  where cs = caminos2 m
        k  = maximum (map sum cs)

caminos2 :: Matrix Int -> [[Int]]
caminos2 m =
  map reverse (matrizCaminos m ! (nrows m, ncols m))

matrizCaminos :: Matrix Int -> Matrix [[Int]]
matrizCaminos m = q
  where
    q = matrix (nrows m) (ncols m) f
    f (1,y) = [[m!(1,z) | z <- [y,y-1..1]]]
    f (x,1) = [[m!(z,1) | z <- [x,x-1..1]]]
    f (x,y) = [m!(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]  

-- 3ª definición (con programación dinámica)
-- =========================================

caminoMaxSuma :: Matrix Int -> [Int]
caminoMaxSuma m = reverse (snd (q ! (nf,nc)))
  where nf = nrows m
        nc = ncols m
        q  = caminoMaxSumaAux m

caminoMaxSumaAux :: Matrix Int -> Matrix (Int,[Int])
caminoMaxSumaAux m = q 
  where
    nf = nrows m
    nc = ncols m
    q  = matrix nf nc f
      where
        f (1,1) = (m!(1,1),[m!(1,1)])
        f (1,j) = (k + m!(1,j), m!(1,j):xs)
          where (k,xs) = q!(1,j-1)
        f (i,1) = (k + m!(i,1), m!(i,1):xs)
          where (k,xs) = q!(i-1,1)        
        f (i,j) | k1 > k2   = (k1 + m!(i,j), m!(i,j):xs)
                | otherwise = (k2 + m!(i,j), m!(i,j):ys)
          where (k1,xs) = q!(i,j-1)
                (k2,ys) = q!(i-1,j)

-- Comparación de eficiencia
-- -------------------------

--    λ> length (caminoMaxSuma1 (fromList 11 11 [1..]))
--    21
--    (10.00 secs, 1,510,120,328 bytes)
--    λ> length (caminoMaxSuma2 (fromList 11 11 [1..]))
--    21
--    (3.84 secs, 745,918,544 bytes)
--    λ> length (caminoMaxSuma3 (fromList 11 11 [1..]))
--    21
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se dice que A es un subconjunto maximal de B si A ⊂ B y
-- no existe ningún C tal que A ⊂ C y C ⊂ B. Por ejemplo, {2,5} es un
-- subconjunto maximal de {2,3,5], pero {3] no lo es.
--
-- El árbol de los subconjuntos de un conjunto A es el árbol que tiene
-- como raíz el conjunto A y cada nodo tiene como hijos sus subconjuntos
-- maximales. Por ejemplo, el árbol de subconjuntos de [2,3,5] es 
--
--           [2, 3, 5]
--           /   |  \
--          /    |   \  
--         /     |    \   
--        /      |     \
--       /       |      \
--     [5,3]   [2,3]   [2,5]  
--     /  \    /  \    /  \  
--    [3] [5] [3] [2] [5] [2]
--     |   |   |   |   |   | 
--    [ ] [ ] [ ] [ ] [ ] [ ]
--
-- Usando el tipo de dato
--    data Arbol = N Integer [Arbol]
--      deriving (Eq, Show)
-- el árbol anterior se representa por
--    N [2,5,3]
--      [N [5,3]
--         [N [3]
--            [N [] []],
--          N [5]
--            [N [] []]],
--       N [2,3]
--         [N [3]
--            [N [] []],
--          N [2]
--            [N [] []]],
--       N [2,5]
--         [N [5]
--            [N [] []],
--          N [2]
--            [N [] []]]]
-- Definir las funciones 
--    arbolSubconjuntos             :: [Int] -> Arbol 
--    nOcurrenciasArbolSubconjuntos :: [Int] -> [Int] -> Int
-- tales que
-- + (arbolSubconjuntos x) es el árbol de los subconjuntos de xs. Por
--   ejemplo,  
--      λ> arbolSubconjuntos [2,5,3]
--      N [2,5,3] [N [5,3] [N [3] [N [] []],N [5] [N [] []]],
--                 N [2,3] [N [3] [N [] []],N [2] [N [] []]],
--                 N [2,5] [N [5] [N [] []],N [2] [N [] []]]]
-- + (nOcurrenciasArbolSubconjuntos xs ys) es el número de veces que
--   aparece el conjunto xs en el árbol de los subconjuntos de ys. Por
--   ejemplo,   
--      nOcurrenciasArbolSubconjuntos []      [2,5,3]  ==  6
--      nOcurrenciasArbolSubconjuntos [3]     [2,5,3]  ==  2
--      nOcurrenciasArbolSubconjuntos [3,5]   [2,5,3]  ==  1
--      nOcurrenciasArbolSubconjuntos [3,5,2] [2,5,3]  ==  1
-- ---------------------------------------------------------------------

data Arbol = N [Int] [Arbol]
  deriving (Eq, Show)

arbolSubconjuntos :: [Int] -> Arbol 
arbolSubconjuntos xs =
  N xs (map arbolSubconjuntos (subconjuntosMaximales xs))

-- (subconjuntosMaximales xs) es la lista de los subconjuntos maximales
-- de xs. Por ejemplo,
--    subconjuntosMaximales [2,5,3]  ==  [[5,3],[2,3],[2,5]]
subconjuntosMaximales :: [Int] -> [[Int]]
subconjuntosMaximales xs =
  [delete x xs | x <- xs]

-- Definición de nOcurrenciasArbolSubconjuntos
-- ===========================================

nOcurrenciasArbolSubconjuntos :: [Int] -> [Int] -> Int
nOcurrenciasArbolSubconjuntos xs ys =
  nOcurrencias xs (arbolSubconjuntos ys)

-- (nOcurrencias x a) es el número de veces que aparece x en el árbol
-- a. Por ejemplo,
--    nOcurrencias 3 (arbolSubconjuntos 30)  ==  2
nOcurrencias :: [Int] -> Arbol -> Int
nOcurrencias xs (N ys [])
  | conjunto xs == conjunto ys  = 1
  | otherwise                   = 0
nOcurrencias xs (N ys zs)
  | conjunto xs == conjunto ys = 1 + sum [nOcurrencias xs z | z <- zs]
  | otherwise                  = sum [nOcurrencias xs z | z <- zs]

-- (conjunto xs) es el conjunto ordenado correspondiente a xs. Por
-- ejemplo, 
--    conjunto [3,2,5,2,3,7,2]  ==  [2,3,5,7]
conjunto :: [Int] -> [Int]
conjunto = nub . sort
