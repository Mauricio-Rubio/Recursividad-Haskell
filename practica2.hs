-- Ejercicios práctica 2. Recursión

--Ejercicio 1
---- Maximo comun divisor
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd 0 b = b 
mcd a b = mcd b(mod a b)
-------------------------------------------------------------------------------

--Ejercicio 2
---- Minimo comun multiplo  a*b = m.c.m(a,b) * m.c.d(a,b)
mcm :: Int -> Int -> Int
mcm a 0 = 0
mcm 0 b = 0
mcm a b =  div (a*b) (mcd a b)
-------------------------------------------------------------------------------

--Ejercicio 3
---- Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (a:xs) = 1 + longitud xs
-------------------------------------------------------------------------------
--Ejercicio 4
---- Máximo de una lista
maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:(y:xs)) = if x > y then x else maximo (y:xs)
-------------------------------------------------------------------------------

--Ejercicio 5
---- Reversa de una lista
reversa :: [a] -> [a]
reversa [x] = [x]
reversa (x:xs) = (reversa xs) ++ [x]
-------------------------------------------------------------------------------

--Ejercicio 6
---- Palíndromo
palindromo :: Eq a => [a] -> Bool
palindromo x = reversa x == x
-------------------------------------------------------------------------------

--Ejercicio 7
---- Divisores
divisoresDesde :: Int -> Int -> [Int]
divisoresDesde n m = if n == m then [n] else if (n > m && (mod n m) == 0) 
    then m:(divisoresDesde n (m+1)) else if (n > m && (mod n m /= 0))
         then divisoresDesde n (m+1) else [0]
-------------------------------------------------------------------------------

--Ejercicio 8
contiene :: Eq a => [a] -> a -> Bool
contiene [] x = False
contiene (x:xs) y = if x == y then True else contiene xs y

negacion :: Bool -> Bool
negacion True = False
negacion False = True

---- Diferencia Simétrica
diferenciaSimetrica :: Eq a =>[a] -> [a] ->[a]
diferenciaSimetrica [] [a] = [a]
diferenciaSimetrica [a] [] = [a]
diferenciaSimetrica xs ys = [x | x <- xs, negacion (contiene ys x)] ++ [x | x <- ys, negacion (contiene xs x)]
-------------------------------------------------------------------------------

--Ejercicio 9 (incompleto)
---- Multiplicación de matrices
--multMatriz :: Num a => [[a]] -> [[a]] -> [[a]]
--multMatriz [] [a] = []
--multMatriz [a] [] = []
--multMatriz [] [] = []
--multMatriz (x:xs) (y:ys) = if(x * y) /= 0 then [] else [1]

--saltarFilas :: Num a => a -> [[a]] -> [a]
--saltarFilas a x(y:xs) = [a*1]
-------------------------------------------------------------------------------
--filasLista :: Num a => [[a]] -> Int
--filasLista [] = 0
--filasLista [x:xs] = if( 1 > 2) then filasLista (xs) else 1 

--Ejercicio 10
---- Conjunto Potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = let conjuntoPotencia_xs = conjuntoPotencia xs
              in conjuntoPotencia_xs++[(x:z) | z <- conjuntoPotencia_xs]
--Creamos una variable en donde se va a almacenar una lista definida por comprensión, y a que a su vez
-- se le va a ir concatenando la misma variable, como en otros lenguajes: +=

