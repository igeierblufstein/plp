
data Polinomio a = X
| Cte a
| Suma (Polinomio a) (Polinomio a)
| Prod (Polinomio a) (Polinomio a)

foldPolinomio :: Num a => b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPolinomio f1 f2 f3 f4 X = f1
foldPolinomio f1 f2 f3 f4 (Cte a) = f2 a
foldPolinomio f1 f2 f3 f4 (Suma p1 p2) = f3 (foldPolinomio f1 f2 f3 f4 p1) (foldPolinomio f1 f2 f3 f4 p2)
foldPolinomio f1 f2 f3 f4 (Prod p1 p2) = f4 (foldPolinomio f1 f2 f3 f4 p1) (foldPolinomio f1 f2 f3 f4 p2)

evaluarFold :: Num a => a -> Polinomio a -> a
evaluarFold x p = foldPolinomio x (id) (+) (*) p
-- x es la constante y f es la funcnion que queres evaluar

evaluar :: Num a => a -> Polinomio a -> a 
evaluar n X = n 
evaluar x (Cte a) = a 
evaluar x (Suma a b) = evaluar x a + evaluar x b
evaluar x (Prod a b) = evaluar x a * evaluar x b 
