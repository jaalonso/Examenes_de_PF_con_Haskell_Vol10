-- Informática (1º del Grado en Matemáticas)
-- Examen de 1º convocatoria (2 de julio de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import I1M.Grafo
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    eliminaUnitarias :: Char -> String -> String
-- tal que (eliminaUnitarias c cs) es la lista obtenida eliminando de la
-- cadena cs las ocurrencias unitarias del carácter c (es decir,
-- aquellas ocurrencias de c tales que su elemento anterior y posterior
-- es distinto de c). Por ejemplo,
--    eliminaUnitarias 'X' ""                  == ""
--    eliminaUnitarias 'X' "X"                 == ""
--    eliminaUnitarias 'X' "XX"                == "XX"
--    eliminaUnitarias 'X' "XXX"               == "XXX"
--    eliminaUnitarias 'X' "abcd"              == "abcd"
--    eliminaUnitarias 'X' "Xabcd"             == "abcd"
--    eliminaUnitarias 'X' "XXabcd"            == "XXabcd"
--    eliminaUnitarias 'X' "XXXabcd"           == "XXXabcd"
--    eliminaUnitarias 'X' "abcdX"             == "abcd"
--    eliminaUnitarias 'X' "abcdXX"            == "abcdXX"
--    eliminaUnitarias 'X' "abcdXXX"           == "abcdXXX"
--    eliminaUnitarias 'X' "abXcd"             == "abcd"
--    eliminaUnitarias 'X' "abXXcd"            == "abXXcd"
--    eliminaUnitarias 'X' "abXXXcd"           == "abXXXcd"
--    eliminaUnitarias 'X' "XabXcdX"           == "abcd"
--    eliminaUnitarias 'X' "XXabXXcdXX"        == "XXabXXcdXX"
--    eliminaUnitarias 'X' "XXXabXXXcdXXX"     == "XXXabXXXcdXXX"
--    eliminaUnitarias 'X' "XabXXcdXeXXXfXx"   == "abXXcdeXXXfx"
-- ---------------------------------------------------------------------

-- 1ª solución (por comprensión):
eliminaUnitarias :: Char -> String -> String
eliminaUnitarias c cs = concat [xs | xs <- group cs, xs /= [c]] 

-- 2ª solución (por composición):
eliminaUnitarias2 :: Char -> String -> String
eliminaUnitarias2 c = concat . filter (/=[c]) . group

-- 3ª solución (por recursión):
eliminaUnitarias3 :: Char -> String -> String
eliminaUnitarias3 _ [] = []
eliminaUnitarias3 c [x] | c == x    = []
                        | otherwise = [x]
eliminaUnitarias3 c (x:y:zs) 
  | x /= c    = x : eliminaUnitarias3 c (y:zs)
  | y /= c    = y : eliminaUnitarias3 c zs
  | otherwise = takeWhile (==c) (x:y:zs) ++ 
                eliminaUnitarias3 c (dropWhile (==c) zs)

-- 4ª solución (por recursión con acumuladores):
eliminaUnitarias4 :: Char -> String -> String
eliminaUnitarias4 c cs = reverse (aux0 cs "")
  where aux0 [] cs2                  = cs2
        aux0 (x:cs1) cs2 | x == c    = aux1 cs1 cs2
                         | otherwise = aux0 cs1 (x:cs2)
        aux1 [] cs2                  = cs2
        aux1 (x:cs1) cs2 | x == c    = aux2 cs1 (c:c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)
        aux2 [] cs2                  = cs2
        aux2 (x:cs1) cs2 | x == c    = aux2 cs1 (c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)

-- 5ª solución (por recursión con span)
eliminaUnitarias5 :: Char -> String -> String
eliminaUnitarias5 c [] = []
eliminaUnitarias5 c cs
  | ys == [c] = xs ++ eliminaUnitarias5 c zs
  | otherwise = xs ++ ys ++ eliminaUnitarias5 c zs
  where (xs,us) = span (/=c) cs
        (ys,zs) = span (==c) us

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir con programación dinámica la función
--    descomposiciones :: Int -> [[Int]]
-- tal que (descomposiciones x) es la lista de las listas de los
-- cuadrados de cuatro números enteros positivos cuya suma es x. Por
-- ejemplo. 
--    λ> descomposiciones 4
--    [[1,1,1,1]]
--    λ> descomposiciones 5
--    []
--    λ> descomposiciones 7
--    [[1,1,1,4],[1,1,4,1],[1,4,1,1],[4,1,1,1]]
--    λ> descomposiciones 10
--    [[1,1,4,4],[1,4,1,4],[1,4,4,1],[4,1,1,4],[4,1,4,1],[4,4,1,1]]
--    λ> descomposiciones 15
--    [[1,1,4,9],[1,1,9,4],[1,4,1,9],[1,4,9,1],[1,9,1,4],[1,9,4,1],
--     [4,1,1,9],[4,1,9,1],[4,9,1,1],[9,1,1,4],[9,1,4,1],[9,4,1,1]]
--    λ> length (descomposiciones 30000)
--    4612
-- ---------------------------------------------------------------------

-- La definición por recursión es
-- ==============================

descomposiciones1 :: Int -> [[Int]]
descomposiciones1 x = aux x 4
  where 
    aux 0 1 = []
    aux 1 1 = [[1]]
    aux 2 1 = []
    aux 3 1 = []
    aux y 1 | esCuadrado y = [[y]]
            | otherwise    = []
    aux y n = [z^2 : zs | z <- [1..raizEntera y]
                        , zs <- aux (y - z^2) (n-1)]

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Int -> Bool
esCuadrado x = (raizEntera x)^2 == x

-- (raizEntera n) es el mayor entero cuya raíz cuadrada es menor o igual
-- que n. Por ejemplo,
--    raizEntera 15  ==  3
--    raizEntera 16  ==  4
--    raizEntera 17  ==  4
raizEntera :: Int -> Int
raizEntera = floor . sqrt . fromIntegral 

-- La definición con programación dinámica es
-- ==========================================

descomposiciones :: Int -> [[Int]]
descomposiciones x = a ! (x,4)
  where
    a = array ((0,1),(x,4)) [((i,j), f i j) | i <- [0..x], j <- [1..4]]
    f 0 1 = []
    f 1 1 = [[1]]
    f 2 1 = []
    f 3 1 = []
    f i 1 | esCuadrado i = [[i]]
          | otherwise    = []
    f i j = [z^2 : zs | z <- [1..raizEntera i]
                      , zs <- a ! (i - z^2,j-1)]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (descomposiciones1 (2*10^4))
--    1068
--    (3.70 secs, 3,307,251,704 bytes)
--    λ> length (descomposiciones (2*10^4))
--    1068
--    (0.72 secs, 678,416,144 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    minimales :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a)
-- tal que (minimales xss) es el conjunto de los elementos de xss que no
-- contienen a ningún otro elemento de xss. Por ejemplo,
--    λ> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
--    λ> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,1],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
--    λ> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,1,3],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
-- ---------------------------------------------------------------------

minimales :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a)
minimales xss = S.filter esMinimal xss
  where esMinimal xs = S.null (S.filter (`S.isProperSubsetOf` xs) xss)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un ciclo en un grafo G es una secuencia de nodos
-- [v(1),v(2),v(3),...,v(n)] de G tal que: 
--    1) (v(1),v(2)), (v(2),v(3)), (v(3),v(4)), ..., (v(n-1),v(n)) son
--       aristas de G, 
--    2) v(1) = v(n), y
--    3) salvo v(1) = v(n), todos los v(i) son distintos entre sí.
-- 
-- Definir la función
--    ciclos :: Grafo Int Int -> [[Int]]
-- tal que (ciclos g) es la lista de ciclos de g. Por ejemplo, si g1 y
-- g2 son los grafos definidos por
--    g1, g2 :: Grafo Int Int
--    g1 = creaGrafo D (1,4) [(1,2,0),(2,3,0),(2,4,0),(4,1,0)]
--    g2 = creaGrafo D (1,4) [(1,2,0),(2,1,0),(2,4,0),(4,1,0)]
-- entonces
--    ciclos g1  ==  [[1,2,4,1],[2,4,1,2],[4,1,2,4]]
--    ciclos g2  ==  [[1,2,1],[1,2,4,1],[2,1,2],[2,4,1,2],[4,1,2,4]]
-- Como se observa en los ejemplos, un mismo ciclo puede tener
-- distintas representaciones según su vértice inicial.
-- ---------------------------------------------------------------------

g1, g2 :: Grafo Int Int
g1 = creaGrafo D (1,4) [(1,2,0),(2,3,0),(2,4,0),(4,1,0)]
g2 = creaGrafo D (1,4) [(1,2,0),(2,1,0),(2,4,0),(4,1,0)]

-- 1ª definición
-- =============
ciclos :: Grafo Int Int -> [[Int]]
ciclos g = 
  sort [ys | (x:xs) <- concatMap permutations (subsequences (nodos g)) 
           , let ys = (x:xs) ++ [x]
           , esCiclo ys g]

-- (esCiclo vs g) se verifica si vs es un ciclo en el grafo g. Por
-- ejemplo, 
esCiclo :: [Int] -> Grafo Int Int -> Bool
esCiclo vs g = 
  all (aristaEn g) (zip vs (tail vs)) &&
  head vs == last vs &&
  length (nub vs) == length vs - 1    

-- 2ª definición
-- =============

ciclos2 :: Grafo Int Int -> [[Int]]
ciclos2 g = sort [ys | (x:xs) <- caminos g
                     , let ys = (x:xs) ++ [x]
                     , esCiclo ys g]

-- (caminos g) es la lista de los caminos en el grafo g. Por ejemplo,
--    caminos g1  ==  [[1,2,3],[1,2,4],[2,3],[2,4,1],[3],[4,1,2,3]]
caminos :: Grafo Int Int -> [[Int]]
caminos g = concatMap (caminosDesde g) (nodos g)

-- (caminosDesde g v) es la lista de los caminos en el grafo g a partir
-- del vértice v. Por ejemplo,
--    caminosDesde g1 1  ==  [[1],[1,2],[1,2,3],[1,2,4]]
--    caminosDesde g1 2  ==  [[2],[2,3],[2,4],[2,4,1]]
--    caminosDesde g1 3  ==  [[3]]
--    caminosDesde g1 4  ==  [[4],[4,1],[4,1,2],[4,1,2,3]]
caminosDesde :: Grafo Int Int -> Int -> [[Int]]
caminosDesde g v = 
  map (reverse . fst) $
  concat $ 
  takeWhile (not.null) (iterate (concatMap sucesores) [([v],[v])])
  where sucesores (x:xs,ys) = [(z:x:xs,z:ys) | z <- adyacentes g x
                                             , z `notElem` ys]
