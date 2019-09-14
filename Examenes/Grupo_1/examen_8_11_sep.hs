-- Informática (1º del Grado en Matemáticas)
-- Examen de 2º convocatoria (11 de septiembre de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1 En la aritmética lunar la suma se hace como en la
-- terrícola salvo que sus tablas de sumar son distintas. La suma lunar
-- de dos dígitos es su máximo (por ejemplo, 1 + 3 = 3 y 7 + 4 = 7). Por
-- tanto,  
--      3 5 7    
--    +   6 4    
--    -------    
--      3 6 7    
--
-- Definir la función
--    suma :: Integer -> Integer -> Integer
-- tal que (suma x y) es la suma lunar de x e y. Por ejemplo, 
--      suma 357 64  ==  367
--      suma 64 357  ==  367
--      suma 1 3     ==  3
--      suma 7 4     ==  7
-- ---------------------------------------------------------------------

suma :: Integer -> Integer -> Integer
suma 0 0 = 0
suma x y = max x2 y2 + 10 * suma x1 y1
  where (x1,x2) = x `divMod` 10
        (y1,y2) = y `divMod` 10

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la suma lunar es
-- conmutativa.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_conmutativa :: Integer -> Integer -> Property
prop_conmutativa x y =
  x >= 0 && y >= 0 ==> suma x y == suma y x 

-- La comprobación es
--    λ> quickCheck prop_conmutativa
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. La sucesión de Fibonacci es
--    0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,...
-- cuyos dos primeros términos son 0 y 1 y los restantes se obtienen
-- sumando los dos anteriores.
--
-- El árbol de computación de su 5º término es
--                  5
--                 / \
--                /   \
--               /     \
--              /       \
--             /         \
--            3           2  
--           / \         / \ 
--          /   \       1   1
--         2     1     / \   
--        / \   / \   1   0  
--       1   1 1   0
--      / \ 
--     1   0  
-- que, usando los árboles definidos por
--    data Arbol = H Int
--               | N Int Arbol Arbol
--      deriving (Eq, Show)
-- se puede representar por
--    N 5              
--      (N 3           
--         (N 2        
--            (N 1 (H 1) (H 0))
--            (H 1))   
--         (N 1 (H 1) (H 0)))  
--      (N 2           
--         (N 1 (H 1) (H 0))   
--         (H 1))     
--
-- Definir la función
--    arbolFib           :: Int -> Arbol
-- tal que (arbolFib n) es el árbol de computación del n-ésimo término
-- de la sucesión de Fibonacci. Por ejemplo,
--      λ> arbolFib 5
--      N 5              
--        (N 3           
--           (N 2        
--              (N 1 (H 1) (H 0))
--              (H 1))   
--           (N 1 (H 1) (H 0)))  
--        (N 2           
--           (N 1 (H 1) (H 0))   
--           (H 1))
--      λ> arbolFib 6
--      N 8
--        (N 5
--           (N 3
--              (N 2
--                 (N 1 (H 1) (H 0))
--                 (H 1))
--              (N 1 (H 1) (H 0)))
--           (N 2
--              (N 1 (H 1) (H 0))
--              (H 1)))
--        (N 3
--           (N 2
--              (N 1 (H 1) (H 0)) (H 1))
--           (N 1 (H 1) (H 0)))
-- ---------------------------------------------------------------------

data Arbol = H Int
           | N Int Arbol Arbol
  deriving (Eq, Show)

-- 1ª definición
-- =============

arbolFib :: Int -> Arbol
arbolFib 0 = H 0
arbolFib 1 = H 1
arbolFib n = N (fib n) (arbolFib (n-1)) (arbolFib (n-2))

-- (fib n) es el n-ésimo elemento de la sucesión de Fibonacci. Por
-- ejemplo,
--    fib 5  ==  5
--    fib 6  ==  8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 2ª definición
-- =============

arbolFib2 :: Int -> Arbol
arbolFib2 0 = H 0
arbolFib2 1 = H 1
arbolFib2 2 = N 1 (H 1) (H 0)
arbolFib2 3 = N 2 (N 1 (H 1) (H 0)) (H 1)
arbolFib2 n = N (a1 + a2) (N a1 i1 d1) (N a2 i2 d2)
  where (N a1 i1 d1) = arbolFib2 (n-1)
        (N a2 i2 d2) = arbolFib2 (n-2)

-- 3ª definición
-- =============

arbolFib3 :: Int -> Arbol
arbolFib3 0 = H 0
arbolFib3 1 = H 1
arbolFib3 2 = N 1 (H 1) (H 0)
arbolFib3 3 = N 2 (N 1 (H 1) (H 0)) (H 1)
arbolFib3 n = N (a + b) i d
  where i@(N a _ _) = arbolFib3 (n-1)
        d@(N b _ _) = arbolFib3 (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. El polinomio cromático de un grafo calcula el número
-- de maneras en las cuales puede ser coloreado el grafo usando un
-- número de colores dado, de forma que dos vértices adyacentes no
-- tengan el mismo color.   
-- 
-- En el caso del grafo completo de n vértices, su polinomio cromático
-- es P(n)(x) = x(x-1)(x-2) ... (x-(n-1)). Por ejemplo, 
--    P(3)(x) = x(x-1)(x-2)      = x^3 - 3*x^2 + 2*x
--    P(4)(x) = x(x-1)(x-2)(x-3) = x^4 - 6*x^3 + 11*x^2 - 6*x
-- Lo que significa que P(4)(x) es el número de formas de colorear el
-- grafo completo de 4 vértices con x colores. Por tanto, 
--    P(4)(2) =  0 (no se puede colorear con 2 colores)
--    P(4)(4) = 24 (hay 24 formas de colorearlo con 4 colores)
--
-- Definir la función 
--    polGC :: Int -> Polinomio Int
-- tal que (polGC n) es el polinomio cromático del grafo completo de n
-- vértices. Por ejemplo,
--    polGC 4  ==  x^4 + -6*x^3 + 11*x^2 + -6*x
--    polGC 5  ==  x^5 + -10*x^4 + 35*x^3 + -50*x^2 + 24*x
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

polGC :: Int -> Polinomio Int
polGC 0 = consPol 0 1 polCero
polGC n = polGC (n-1) `multPol` consPol 1 1 (consPol 0 (-n+1) polCero)

-- 2ª solución
-- ===========

polGC2 :: Int -> Polinomio Int
polGC2 n = multLista (map polMon [0..n-1])

-- (polMon n) es el monomio x-n. Por ejemplo,
--    polMon 3  ==  1*x + -3
polMon :: Int -> Polinomio Int
polMon n = consPol 1 1 (consPol 0 (-n) polCero)

-- (multLista ps) es el producto de la lista de polinomios ps.
multLista :: [Polinomio Int] -> Polinomio Int
multLista []     = polUnidad
multLista (p:ps) = multPol p (multLista ps)

-- 3ª solución (por plegado)
-- =========================

polGC3 :: Int -> Polinomio Int
polGC3 n = foldl multPol polUnidad
           [consPol 1 1 (consPol 0 (-i) polCero) | i <- [0..n-1]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que si el número de colores
-- (x) coincide con el número de vértices del grafo (n), el número de
-- maneras de colorear el grafo es n!.  
-- 
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_polGC
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_polGC :: Int -> Property
prop_polGC n = 
  n > 0 ==> valor (polGC n) n == product [1..n]

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_polGC
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Una partición de un conjunto A es un conjunto de
-- subconjuntos no vacíos de A, disjuntos dos a dos y cuya unión es
-- A. Por ejemplo, el conjunto {1, 2, 3} tiene exactamente 5
-- particiones:  
--    {{1}, {2}, {3}}
--    {{1,2}, {3}}
--    {{1,3}, {2}}
--    {{1}, {2,3}}
--    {{1,2,3}}
--
-- Definir la función
--    nParticiones :: [a] -> Integer
-- tal que (nParticiones xs) es el número de particiones de xs. Por
-- ejemplo,  
--    nParticiones [1,2]                     ==  2
--    nParticiones [1,2,3]                   ==  5
--    nParticiones "abcd"                    ==  15
-- ---------------------------------------------------------------------


-- 1ª definición
-- =============

nParticiones :: [a] -> Integer
nParticiones xs = sum [a ! (n,k) | k <- [0..n]]
  where n = genericLength xs
        a = matrizNParticiones n

-- (matrizNParticiones n) es la matriz de dimensión ((0,0),(n,n)) que en
-- la posición (i,j) tiene el número de particiones de un conjunto de i
-- elementos en j subconjuntos. Por ejemplo,
--    λ> matrizNParticiones 3
--    array ((0,0),(3,3))
--          [((0,0),0),((0,1),0),((0,2),0),((0,3),0),
--           ((1,0),0),((1,1),1),((1,2),0),((1,3),0),
--           ((2,0),0),((2,1),1),((2,2),1),((2,3),0),
--           ((3,0),0),((3,1),1),((3,2),3),((3,3),1)]
--    λ> matrizNParticiones 4
--    array ((0,0),(4,4))
--          [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((0,4),0),
--           ((1,0),0),((1,1),1),((1,2),0),((1,3),0),((1,4),0),
--           ((2,0),0),((2,1),1),((2,2),1),((2,3),0),((2,4),0),
--           ((3,0),0),((3,1),1),((3,2),3),((3,3),1),((3,4),0),
--           ((4,0),0),((4,1),1),((4,2),7),((4,3),6),((4,4),1)]
matrizNParticiones :: Integer -> Array (Integer,Integer) Integer
matrizNParticiones n = a 
  where
    a = array ((0,0),(n,n)) [((i,j), f i j) | i <- [0..n], j <- [0..n]]
    f 0 0 = 1
    f 0 _ = 0
    f _ 0 = 0
    f _ 1 = 1
    f i j = a ! (i-1,j-1) + j * a ! (i-1,j)

-- 2ª definición
-- =============

nParticiones2 :: [a] -> Integer
nParticiones2 xs = sum [a ! (n,k) | k <- [0..n]]
  where
    n = genericLength xs
    a = array ((0,0),(n,n)) [((i,j), f i j) | i <- [0..n], j <- [0..n]]
    f 0 0 = 1
    f 0 _ = 0
    f _ 0 = 0
    f _ 1 = 1
    f i j = a ! (i-1,j-1) + j * a ! (i-1,j)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular cuántos dígitos tiene el número de
-- particiones del conjunto con 600 elementos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> length (show (nParticiones [1..600]))
--    1050
--    (0.93 secs, 675,285,608 bytes)
--    λ> length (show (nParticiones2 [1..600]))
--    1050
--    (0.92 secs, 675,287,656 bytes)
