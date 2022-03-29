{-
    Crie uma função em Haskell alocar::[Int]->Int->[[Int]] que recebe uma lista de inteiros e um inteiro n e ela deve retornar uma lista com listas de n elementos de forma que os elementos nas sublistas são inseridos de forma consecutiva varrendo os elementos da lista principal. Por exemplo: 

    alocar [4,23,2,5,6,8,12,57] 3 = [[4,23,2],[5,6,8],[12,57]] 
-}

import Data.List

alocar :: [Int] -> Int -> [[Int]]
alocar xs n = reparteLista n xs

reparteLista :: Int -> [Int] -> [[Int]]
reparteLista _ [] = []
reparteLista n xs = as : reparteLista n bs 
  where (as,bs) = splitAt n xs