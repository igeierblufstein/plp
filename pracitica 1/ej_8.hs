-- Ejercicio 8
-- III )
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun criterio = foldr1 (\x mejorActual -> if criterio x mejorActual then x else mejorActual) 


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