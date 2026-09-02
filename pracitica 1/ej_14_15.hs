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