
data AIH a = Hoja a | Bin (AIH a) (AIH a)

-- diferencias con AB, el caso base es un constructor, y en la recursion
-- no recibe una raiz

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH z f t = case t of 
    Hoja r -> z r  
    Bin i d -> f (foldAIH z f i) (foldAIH z f d)

altura :: AIH a -> Integer 
altura = foldAIH (\_ -> 1) (\ri rd -> 1 + max ri rd)
-- como z tiene que ser una funcion hacer (\_ -> 1)

tamano :: AIH a -> Integer -- cantidad de hojas
tamano = foldAIH (\_ -> 1) (\ri rd -> ri + rd)

infinitoAIH :: [AIH ()]
infinitoAIH = [x |hojas <- [1..], x <- generarAIH hojas]

generarAIH :: Int -> [AIH ()]
generarAIH 1 = [Hoja()]
generarAIH n = [Bin i d | k <- [1..n-1], d <- generarAIH k, i <- generarAIH (n-k)]


a1 :: AIH Int
a1 = Hoja 5

a2 :: AIH Int
a2 = Bin (Hoja 5) (Hoja 8)

a3 :: AIH Int
a3 = Bin
        (Hoja 5)
        (Bin (Hoja 8) (Hoja 10))

