{-
    Segundo Helder Nakaya, membro do comitê científico da SBI (Sociedade Brasileira de Infectologia), para medir a eficácia de uma vacina "o que se faz é comparar o número de infectados no grupo de controle - aquele que recebe placebo ou outra vacina que não é a que está sendo testada - , com o número de infectados no grupo que recebeu a vacina em desenvolvimento". Por exemplo, considerando 20% dos voluntários que receberam a vacina tiveram a doença, e que 70% dos que receberam o placebo adoeceram, a conta a ser feita é a seguinte: um menos o valor resultante da divisão de 20% por 70%. Neste caso hipotético, a eficácia é de 71,4%. Assim, crie um programa em Haskell que lê do usuário o nome da vacina, a porcentagem de pessoas que se vacinaram e tiveram a doença, e a porcentagem dos que tomaram placebo e tiveram a doença. O processo se repete até que se digite a string vazia para o nome da vacina. Por último, seu programa deve imprimir a média, considerando todas as vacinas lidas do usuário, dos que tomaram a vacina e se infectaram, e também a média dos que tomaram placebo e se infectaram.
-}

{-
    Explicando como funciona;
    ================================================================
    A função relatorioVacina é a principal;
    Digite o nome da vacina,                        ENTER;
    Digite a taxa de contaminados vacinados,        ENTER;
    Digite a taxa de contaminados com placebo,      ENTER;
    Repita o processo até enjoar;
    Quando enjoar, aperte ENTER ao ser solicitado o nome da vacina, sem espaços nem nada;
    ================================================================
-}

import Data.Char

relatorioVacina = do 
                
        putStrLn "Digite o nome da vacina: "                    
        nome <- getLine
        if(nome /= "")
            then do putStrLn "Qual porcentagem de contaminados que tomaram essa vacina?: "
                    contaminadosVacina <- getLine
                    putStrLn "Qual porcentagem de contaminados que tomaram placebo?: "
                    contaminadosPlacebo <- getLine
                    appendFile "eficaciaVacinas.txt" ( nome ++ " " ++ contaminadosVacina ++ " " ++ contaminadosPlacebo ++ "\n")
                    relatorioVacina

        else imprimeResultado 

mediaEficaciaVacina :: [String] -> Float
mediaEficaciaVacina [] = 0.0
mediaEficaciaVacina (x:xs) = fromIntegral(stringParaInt ((split ' ' x) !! 1)) + (mediaEficaciaVacina xs)

mediaEficaciaPlacebo :: [String] -> Float
mediaEficaciaPlacebo [] = 0.0
mediaEficaciaPlacebo (x:xs) = fromIntegral(stringParaInt ((split ' ' x) !! 2)) + (mediaEficaciaPlacebo xs)

stringParaInt :: String -> Int
stringParaInt x = read x::Int

imprimeResultado = do
    c <- readFile "eficaciaVacinas.txt"
    putStrLn( "\n\nmedia eficacia vacina: "++ (show ((mediaEficaciaVacina (lines c)) / fromIntegral(totalLinhas c))))
    putStrLn ("\nmedia eficacia placebo: " ++ (show ((mediaEficaciaPlacebo (lines c)) / fromIntegral(totalLinhas c))))

totalLinhas :: String -> Int
totalLinhas l = length(filter (/= "\n") (lines l))        

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s