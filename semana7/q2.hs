{-
    Crie uma função em Haskell baldear::[Int]->Int->[[Int]] que recebe uma lista 
    de inteiros e um inteiro n e ela deve retornar uma lista com n listas de forma 
    que os elementos nas sublistas são inseridos de forma consecutiva varrendo o
    s elementos da lista principal. Por exemplo: 
    
    baldear [4,23,2,5,6,8,12,57] 3 = [[4,5,12],[23,6,57],[2,8]] 
-}

baldear :: [Int] -> Int -> [[Int]]
baldear [] _ = []
baldear x 0 = []
baldear l n = aux (alocar l n) (n-1) 0

aux :: [[Int]] -> Int -> Int -> [[Int]]
aux l n m | n == m = [[ x !! n | x <- l, length x > n]]
aux l n m = [[ x !! m | x <- l, length x > m]] ++ (aux l n (m + 1))

alocar :: [Int] -> Int -> [[Int]]
alocar xs n = reparteLista n xs

reparteLista :: Int -> [Int] -> [[Int]]
reparteLista _ [] = []
reparteLista n xs = as : reparteLista n bs 
  where (as,bs) = splitAt n xs