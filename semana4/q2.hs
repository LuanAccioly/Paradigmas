{-
Utilizando a linguagem funcional Haskell, defina uma função que recebe uma lista de listas de elementos de um tipo t (genérico) e retorna 
uma lista de tuplas-2 onde o primeiro elemento é um valor do tipo t que existe em pelo menos uma das sub-listas da entrada e o segundo 
é o número de ocorrências desse valor nas sub-listas.
Exemplos: contaOcorr ["haskell","eh","legal"] -> [('h',2),('a',2),('s',1),('k',1),('e',3),('l',4),('g',1)]
contaOcorr [[2,45,6,2,1],[7,7,4,3,2]] -> [(2,3),(45,1),(6,1),(1,1),(7,2),(4,1),(3,1)]
-}

import Data.Char
import Data.Typeable

--Função principal
contaOcorr :: Eq a => [[a]] -> [(a, Int)]
contaOcorr l = removeDuplicados (aux (concat l))

aux :: Eq a => [a] -> [(a, Int)]
aux l = [(x, contador(l, x)) | x <- l]

contador :: Eq a => ([a], a) -> Int
contador (l, x) = length((filter (==x) l))


removeDuplicados::Eq a => [a]->[a]
removeDuplicados [] = []
removeDuplicados (x:xs) = x: (removeDuplicados (remove x xs))
    where
        remove::Eq a => a -> [a] -> [a]
        remove x [] = []
        remove x (y:ys) | x==y = remove x ys
                        | otherwise = y:(remove x ys)




