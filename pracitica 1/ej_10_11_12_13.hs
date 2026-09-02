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