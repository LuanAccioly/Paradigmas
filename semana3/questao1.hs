{-
Suponha que precisamos calcular 2 elevado a um número n. Se n é par, por exemplo 2*m, então: 2^n = 2^(2*m) = (2^m)^2

Se n é ímpar , por exemplo 2*m+1, então: 2^n = 2^(2*m+1) = ((2^m)^2)*2

Desta forma, crie uma função recursiva "eleva2:: Int -> Int" que computa 2 elevado a um número n usando estas ideias e sem usar o operador de potência de Haskell (^).
-}

eleva2 :: Int -> Int
eleva2 0 = 1
eleva2 1 = 2
eleva2 y | (mod y 2) == 0 = eleva (eleva 2 (aux y)) 2
         | otherwise = (eleva (eleva 2 (aux y)) 2) * 2

eleva :: Int -> Int -> Int
eleva 1 y = 1
eleva x 1 = x
eleva x y = x * eleva x (y-1)

aux x = div x 2