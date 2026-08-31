import Data.List(nub)

-- PRACTICA 1




-- Ejercicio 8
--IV)
sumarParciales :: Num a => [a] -> [a]
sumarParciales [x] = [x]
sumarParciales (x:xs) = x : map (+x) (sumarParciales xs)

-- V)
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0 

-- VI)

sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (\ acc x -> x - acc) 0 
-- sumaAltInv = folfl (flip (-)) 0

-- VII)
-- cual seria el tipo
componerTodas :: [a->a] -> a -> a 
componerTodas = foldr (.) id

-- EJjercicio 9
-- I)
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = concatMap agregarEnCadaPos (permutaciones(xs))
    where 
        agregarEnCadaPos perm = [ take i perm ++ [x] ++ drop i perm   | i <- [0.. length perm]]

-- si yo hiciera map supongamos que tengo que agregar 1 en
-- las permutaciones de [[2,3],[3,2]]
-- map agregarEnCadaPos [[2,3],[3,2]] hace
-- [ [[1,2,3],[2,1,3],[2,3,1]] , [[1,3,2],[3,1,2],[3,2,1]] ]
-- entonces concatMap te devuelve 
-- [ [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1] ]

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap f xs = concat (map f xs)

-- por cada elemento de xs, f te devuelve una lista y junta todos
-- los resultados
-- take :: Int -> [a] -> [a] 
-- drop :: Int -> [a] -> [a] 

-- II)
-- partes [5, 1, 2] -> [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = concatMap agregar (partes xs) 
    where 
        agregar p = [p, x : p ]
        -- : es del tipo a -> [a] -> [a]
        -- no podes pasar x segundo por que no [a]


-- III)
-- prefijos [5, 1, 2] -> [[], [5], [5, 1], [5, 1, 2]]
prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos (x:xs) = [] : concatMap agregarPre (prefijos xs)
    where 
        -- agregue al ultimo y uno mas
        agregarPre p = [x:p]

prefijos2:: [a] -> [[a]]
prefijos2 [] = [[]]
prefijos2 (xs) = [take i xs | i <- [1..length xs]]

-- IV)
sublistas ::  [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = concatMap agregar2 (sublistas xs)
    where 
        agregar2 p = [p, x:p] 

sublistasFunciona :: Eq a => [a] -> [[a]]
sublistasFunciona xs = nub [take i (drop j xs) | j <- [0..length xs], i <- [j .. length xs] ]

-- sublistas [5,1,2] = concatMap agregar sublistas [1,2]
-- sublistas [1,2] = concatMap agregar sublistas [2]
-- sublistas [2] = concatMap agregar [[]]
-- agregar [[]] = [2, []]
-- agregar [2,[]] 1 = [[1,2], [2], [1], []]
-- agregar 

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- EJERCICIO 10 

-- CHEQUEAR FUNCIONAMIENTO, SIRVE ENTENDER RECR
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if n == x then xs else x : rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if e < x then e:x:xs else x : rec) [e]

--insertarOrdenado e [] = (\e -> [e])
--insertarOrdenado e (x:xs) = (\e x xs -> if e < x then e:x:xs else x: insertar e xs)

-- EJERCICIO 11
-- ANOTACIONES
    -- COMO HAY DOS CASOS BASE, GENERALIZAS PARA \_ -> []
    -- PASAS TRUE COMO OTRO PARAMETRO PARA VER SI ES PAR O NO
elementosEnPosPares :: [a] -> [a]
elementosEnPosPares xs = foldr (\x acc par -> if par then x : acc False else acc True) (\_ -> []) xs True

-- sufijos es estructural pero como rompes los huevos la hiciste con recr
sufijos :: [a] -> [[a]]
sufijos = recr (\x xs rec -> [x:xs] ++ rec) [[]]

sufijosFoldr :: [a] -> [[a]]
sufijosFoldr = foldr (\x acc -> (x : head acc) : acc) [[]]
    -- NOTAR QUE ACC ES UNA LISTA DE LISTAS ENTONCES HEADD ACC AGREGA A LA LISTA

-- NECESITO UN FOLDR CON DOBLE ENTRADA
-- REPASAR APLICACION DE FUNCIONES PARCIALES
entrelazar :: [a] -> [a]-> [a]
entrelazar [] = id
entrelazar (x:xs)= (\ys -> if null ys then x : entrelazar xs [] else x:(head ys): entrelazar xs (tail ys))

-- INTERESANTE IMPORTANTE ACORDATE ANOTATE 
entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr = foldr combinar id
    where
        combinar x acc = \ys -> if null ys then x : acc [] else x :head ys : acc (tail ys)


-- slowSort ES RECURSION GLOBAL NO SE PUEDE REESCRIBIR CON RECR O FOLDR 
-- PORQUE NI IDEA hace dos cosas paralela al mismo tiempo
slowSort :: Ord a => [a] -> [a]
slowSort [] = []
slowSort (p:xs) = slowSort menores ++ [p] ++ slowSort mayores
    where 
        menores = [x | x <- xs, x <= p]
        mayores = [x | x <- xs, x > p]

-- miScanr 
miScanrFold :: (a -> b -> b) -> b -> [a] -> [b]
miScanrFold f n = foldr(\x xs -> f x (head xs) : xs) [n] 

-- EJERCICIO 12
curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x,y)

uncurryy :: (a -> b -> c) -> (a,b) -> c
uncurryy f (x, y) = f x y

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = foldr(\x xs -> uncurryy f x : xs) []

-- se llama Zip
armarPares :: [a] -> [b] -> [(a,b)]
armarPares xs ys = foldr juntar (\_ -> []) xs ys -- APLICACINO PARCIAL DE FUNCIONES
    where                  -- este es el caso base, devuelve una lista vacia 
        juntar x acc [] = []
        juntar x acc (z:zs) = (x,z) : acc zs

-- se llama zipWith
mapDoble :: (a->b->c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)


-- OTRAS ESTRUCTURAS DE DATOS
-- Ejercicio 14
data Nat = Zero | Succ Nat
-- interesante el tipo Nat, cuando ejecute le tendrias 
-- que pasar Succ (succ (succ Zero)) o hacer un conversor de
-- Integer a Nat

foldNat :: (a -> a) -> a -> Integer -> a
foldNat f z 0 = z
foldNat f z n = f (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia n p = foldNat (\acc-> n * acc ) 1 p

-- Ejercicio 15
-- INTERESANTE PASAR UNA TUPLA DE VALORES
genLista :: a -> (a -> a) -> Integer -> [a]
genLista n f x = reverse (snd (foldNat paso (n,[]) x))
    where
        paso (act,acc) = (f act, act : acc)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) (y-x+1)

-- Ejercicio 16
data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a) 


-- como haria folPolinomio 

evaluar :: Num a => a -> Polinomio a -> a 
evaluar n X = n 
evaluar x (Cte a) = a 
evaluar x (Suma a b) = evaluar x a + evaluar x b
evaluar x (Prod a b) = evaluar x a * evaluar x b 

-- Ejercicio 17
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f t = case t of
    Nil -> z 
    Bin i r d -> f (foldAB z f i) r (foldAB z f d)

--recAB :: b -> 

esNil :: AB a -> Bool 
esNil Nil = True
esNil (Bin i r d) = False

altura :: AB a -> Int 
altura (Bin i r d) = foldAB 0 (\i r d -> 1 + max i d) (Bin i r d) 

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ i r d -> 1 + i + d)

mejorSegun :: (a -> a -> Bool) -> AB a -> a



-- esABB ::  Ord a => AB a -> Bool
