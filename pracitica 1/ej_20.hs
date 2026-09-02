
data RoseTree a = Rose a [RoseTree a]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose t hijos) = f t [foldRose f  x | x <- hijos]

hojas :: RoseTree a -> [a]
hojas = foldRose f
  where
    f t [] = [t]
    f _ recHijos = concat recHijos

distancias :: RoseTree a -> [Int]
distancias = foldRose f 
    where 
        f t [] = [0]
        f _ recHijos = concatMap (map(+1)) recHijos 
                            -- (map(+1)) le quiero hacer a cada
                            -- lista de distancias, 
                            -- ese concatMap le va a aplicar map(+1) 
                            -- a cada lista 
    

altura :: RoseTree a -> Int
altura = foldRose f
    where 
        f t [] = 1 
        f t recHijos = 1 + maximo recHijos 

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun criterio = foldr1 (\x mejorActual -> if criterio x mejorActual then x else mejorActual) 

maximo :: [Int] -> Int 
maximo = mejorSegun (>) 
