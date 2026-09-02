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