{-
Escreva a funcao ocorreUma :: [Int] -> [Int]  em Haskell, que 
deve retornar uma lista com os números inteiros que aparecem 
apenas uma vez na lista passada como parâmetro. 
Ex: ocorreUma [4,1,5,4,3,5]  deve retornar [1,3].

remove os valores iguais ao passado como parametro

-}

removeIguais :: Int -> [Int] -> [Int]
removeIguais num [] = []
removeIguais num (x:xs) 
        | x == num = removeIguais num xs
        | otherwise = [x] ++ removeIguais num xs


{-remove os inteiros repetidos -}

ocorreUma :: [Int] -> [Int]
ocorreUma [] = []
ocorreUma (x:xs)
        | elem x xs = ocorreUma (removeIguais x xs)
        | otherwise = [x] ++ ocorreUma xs

{-

Exercício feito de maneira correta
Utiliza uma função que só retorna os valores da lista que são únicos. Se não houver nenhum valor único, retorna uma lista vazia

Casos testados:

ocorreUma [4,1,5,4,3,5] = [1,3]         / Correto
ocorreUma [1,1,2,2]     = []            / Correto
ocorreUma []            = []            / Correto
ocorreUma [1,3,3,2,5,6] = [1,2,5,6]     / Correto

-}
