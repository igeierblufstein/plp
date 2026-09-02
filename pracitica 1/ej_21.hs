
data HashSet a = Hash (a -> Integer) (Integer -> [a])

vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (\_ -> [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece e (Hash h t) =  elem e (t (h e))

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar e (Hash h t)= if pertenece e (Hash h t) 
    then (Hash h t) 
    else Hash h (\i -> if i == (h e) then e : t i else t i)
-- vos queres que cuando le pases un int, si este es el mismo que h e
-- te te devuelva la misma lista de t (h e) con el nuevo elemento
-- si es distinto, te devuelve la aplicacion original de t i 


interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a 
interseccion (Hash h1 t1) s2 = Hash h1 (\i -> filter(\x -> pertenece x s2) (t1 i))
-- me pasan el numero de un bucket

foldr1 :: (a -> a -> a) -> [a] -> a 
foldr1 f xs = case foldr paso Nothing xs of
    Nothing -> error "lista vacía"
    Just r -> r 
    where 
        paso x [] = Just x
        paso x (Just recu) = Just (f x recu)
