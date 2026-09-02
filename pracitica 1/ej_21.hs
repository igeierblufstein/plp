
data HashSet a = Hash (a -> Integer) (Integer -> [a])

vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (\_ -> [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece e (Hash h t) =  elem e (t (h e))
