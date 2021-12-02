maximoParDivisible :: Integer -> Integer
maximoParDivisible n = maximo (paresDivisibles n [1..n])

-- Funciones permitidas
-- mod a b
-- sqrt a

paresDivisibles :: Integer -> [Integer] -> [Integer]
paresDivisibles n xs = [x | x <- xs, mod n x == 0, mod x 2 == 0]

reversa :: [a] -> [a] 

-- Eq : comparable
-- Ord : ordenable
-- => : condiciona lo que esta del lado izquierdo
-- Palindromo
palindromo :: Eq a => [a] -> Bool
palindromo x = reversa x == x

maximo :: Ord a => [a] -> a
maximo (x:xs) = if x > maximo xs then x else maximo xs

funcionAuxiliar :: Integer -> Integer -> (Integer, Integer)
funcionAuxiliar x y 