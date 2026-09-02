-- Ejercicio 17
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f t = case t of
    Nil -> z 
    Bin i r d -> f (foldAB z f i) r (foldAB z f d)

recAB :: b -> (b -> AB a -> a -> AB a -> b ->b) -> AB a -> b
recAB z f Nil = z
recAB z f (Bin i r d) = f (recAB z f i) i r d (recAB z f d)

esNil :: AB a -> Bool 
esNil Nil = True
esNil (Bin i r d) = False

altura :: AB a -> Int 
altura (Bin i r d) = foldAB 0 (\i r d -> 1 + max i d) (Bin i r d) 

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ i r d -> 1 + i + d)

-- data Maybe a = Nothing | Just a

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f t = case foldAB Nothing mejor t of -- z = Nothing f = mejor 
    Just m -> m
    -- no definimos el caso Nothing para que no se rompa
    where 
        mejor ri r rd = Just (mejorEntre f (mejorEntre f r rd) ri)
-- ojo con los parametro que recibe mejorEntre
-- tiene que recibir uno de tipo a y un maybe

mejorEntre :: (a -> a -> Bool) -> a -> Maybe a -> a
mejorEntre _ x Nothing  = x
mejorEntre f x (Just y) = if f x y then x else y

--  en un árbol binario de búsqueda, el valor de un nodo es mayor 
-- o igual que los valores que aparecen en el subárbol izquierdo y es 
-- estrictamente menor que los valores que aparecen en el subárbol derecho.
-- esABB ::  Ord a => AB a -> Bool

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe casoNothing casoJust m` — si `m` es `Nothing`, devuelve `casoNothing`; si es `Just x`, aplica la función `casoJust` a `x`.

-- ojo con los parametro que recibe mejorEntre
-- tiene que recibir uno de tipo a y un maybe

esABB :: Ord a => AB a -> Bool
esABB t = primero (foldAB (True, Nothing, Nothing) chequeo t) 
    where  
        primero (p,_,_) = p
        chequeo (bri, rimin, rimax) r (brd, rdmin, rdmax) = (bri && brd && cumpleIzq && cumpleDer, nuevoMin, nuevoMax)
            where
                cumpleIzq = maybe True (\m -> m <= r) rimax
                cumpleDer = maybe True (\m -> m > r) rdmin
                nuevoMin = Just (maybe r id rimin)
                nuevoMax = Just (maybe r id rdmax)

ramas :: AB a -> Int
ramas t = maybe 0 (id) (foldAB Nothing caminos t) 
    where 
        caminos ri r rd = Just (sumar ri rd)
        sumar Nothing Nothing = 1
        sumar (Just k) Nothing = k
        sumar Nothing (Just k) = k
        sumar (Just i) (Just d) = i+d

cantHojas :: AB a -> Int
cantHojas t = maybe 0 (id) (foldAB Nothing hojas t)
    where 
        hojas ri r rd = if esHoja ri r rd then Just (1) else Just (sumarHojas ri rd)
        esHoja Nothing r Nothing = True
        sumarHojas (Just k) Nothing = k
        sumarHojas Nothing (Just k) = k
        sumarHojas (Just i) (Just d) = i + d

espejo :: AB  a -> AB a 
espejo = foldAB Nil (\ri r rd -> Bin rd r ri )

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura t p = foldAB esNil comparar t p
    where
        comparar ri r rd segundo = case segundo of 
            Nil -> False 
            Bin idos rdos ddos -> ri idos && rd ddos
