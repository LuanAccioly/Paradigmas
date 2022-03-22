{-
    Na matemática temos três tipos compostos são Set (conjuntos), Bag (sacolas) e Seq (Sequências). 
    As características deles é que Sets não possuem ordem nem elementos repeditos, Bags podem ter elementos repetidos,
     mas não tem ordem, e as Seqs são tipos compostos que permitem a repetição de elementos e que a ordem deles importa, 
     ou seja, são com as listas de Haskell. Crie um programa em Haskell que define um tipo algébrico CompositeType 
     que guarda elementos genéricos representar os três tipos descritos anteriormente, e cada tipo específico 
     usa uma lista como valor. Em seguida, crie uma função "getElements" que dado um CompositeType retorna a lista de 
     elementos do tipo composto de acordo com as seguintes regras:

    - Caso seja um Set, a lista retornada não pode ter elementos repetidos

    - Caso seja um Bag, o que deve ser retornado é uma lista de tuplas onde o primeiro elemento é um item da lista, e o segundo é 
    a quantidade que aquele item aparece na lista. Importante, a lista resultante não pode ter duas ou mais tuplas do mesmo elemento. 

    - Caso seja um Seq, deve retornar a lista do jeito que ela está definida. 

    Dica: O tipo do resultado da função "getElements" pode também ser um tipo algébrico, já que o retorno da função é polimórfico. 
-}


data TiposCompostos a = Bag [a] |  Seq [a] | Set [a]  deriving (Eq)
data RetornoComposto a = Sacola [(a, Int)] | Sequencia [a] | Conjunto [a] deriving (Show, Eq)

--Função principal
getElements :: Eq a => TiposCompostos a -> RetornoComposto a
getElements (Seq x) = Sequencia x
getElements (Set x) = Conjunto (removeDuplicados x)
getElements (Bag x) = Sacola (removeDuplicados (aux x))

removeDuplicados :: Eq a => [a] -> [a]
removeDuplicados [] = []
removeDuplicados (x:xs) = x: (removeDuplicados (remove x xs))
    where
        remove:: Eq a => a -> [a] -> [a]
        remove x [] = []
        remove x (y:ys) | (x == y) = remove x ys
                        | otherwise = y:(remove x ys) 

aux :: Eq a => [a] -> [(a, Int)]
aux l = [(x, contador(l, x)) | x <- l]

contador :: Eq a => ([a], a) -> Int
contador (l, x) = length((filter (==x) l))
