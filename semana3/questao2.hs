{-
Dada as funções "vendas" e "totalVendas" que utilizamos nas aulas desta semana (videoaula e slides), crie uma função "relatorio::Int -> (Int, Int, Int, Float)" que retorna um relatório dos dados até uma determinada semana passada como parâmetro. O relatório é uma tupla onde o primeiro elemento é o total de vendas até aquela semana, o segundo é o número da semana com mais vendas até aquela semana, o terceiro é a maior quantidade de vendas até aquela semana, e o quarto é a média de vendas até aquela semana. Dica: use funções intermediárias para calculcar o que se deseja. 
-}

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 15
vendas 2 = 5
vendas 3 = 20



relatorio :: Int -> (Int, Int, Int, Float)
relatorio n | (n < 0) || (n > 3) = (0, 0, 0, 0)
            | otherwise = (totalVendas n, semanaComMaisVendas n, maisVendas n, mediaVendasAteN n)

--o primeiro elemento é o total de vendas até aquela semana
totalVendas n | n == 0 = vendas 0
              | otherwise = vendas n + totalVendas(n-1)


--o segundo é o número da semana com mais vendas até aquela semana
semanaComMaisVendas :: Int -> Int
semanaComMaisVendas 1 | vendas 1 >= vendas 0 =  1
                      | otherwise = 0
semanaComMaisVendas n | vendas n >= maisVendas (n-1) = n
                      | otherwise = semanaComMaisVendas (n-1)


--o terceiro é a maior quantidade de vendas até aquela semana
maisVendas :: Int -> Int 
maisVendas 1 | vendas 1 >= vendas 0 = vendas 1
             | otherwise = vendas 0
maisVendas n | vendas n >= maisVendas (n-1) = vendas n
             | otherwise = maisVendas (n-1)

--o quarto é a média de vendas até aquela semana
mediaVendasAteN n | n == 0 = fromIntegral(vendas 0)
                  | otherwise = fromIntegral(totalVendas n) / fromIntegral(n + 1)




  


