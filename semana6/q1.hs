{-
    Crie um programa em Haskell que lê informações do teclado e ao final compila as informações recebidas como entrada em um arquivo de texto. Seu programa deverá repetidamente fazer as seguintes perguntas:

    1 - Informe seu nome; R - string com o nome

    2 - Você está com confirmação de diagnóstico de infecção por COVID - 19? R - 's' para sim e 'n' para não.

    3 - Você coabita com pessoas com diagnóstico ou suspeita de infecção por COVID-19? R - 's' para sim e 'n' para não.

    Seu programa deve repetir o processo de perguntas até que a string vazia seja informada na pergunta 1. Em seguida, seu programa deve gerar um arquivo "covid.txt" onde cada linha deve ter as informações de cada pessoa que respondeu e após listar as pessoas, as 3 últimas linhas devem ter as seguintes informações:

    - totais de pessoas que responderam o questionário

    - média de pessoas com confirmação de diagnóstico de infecção por COVID - 19

    - média de pessoas que coabitam com pessoas com diagnóstico ou suspeita de infecção por COVID-19.
-}

import Data.List

relatorioCovid = do 
                
        putStrLn "Digite seu primeiro nome: "                    
        nome <- getLine
        if(nome /= "")
            then do putStrLn "Você está com confirmacao de diagnostico de infeccao por COVID - 19? R - 's' para sim e 'n' para nao."
                    confirmadoCovid <- getLine
                    putStrLn "Voce coabita com pessoas com diagnostico ou suspeita de infeccao por COVID-19? R - 's' para sim e 'n' para nao."
                    coabitaInfectados <- getLine
                    appendFile "relatorio.txt" ( nome ++ " " ++ confirmadoCovid ++ " " ++ coabitaInfectados ++ "\n")
                    relatorioCovid

        else aux    

mediaInfectados :: String -> Float
mediaInfectados l = fromIntegral(length ([x | x <- (lines l), (isInfixOf "s s" x) || (isInfixOf "s n" x)])) / fromIntegral(totalRespostas l)

mediaConvivio :: String -> Float
mediaConvivio l = fromIntegral(length ([x | x <- (lines l), (isInfixOf "s s" x) || (isInfixOf "n s" x)])) / fromIntegral(totalRespostas l)

aux = do 
    c <- readFile "relatorio.txt"
    appendFile "covid.txt" ( c ++ "\nTotal respostas: " ++ (show (totalRespostas c)) ++ "\nMedia contaminados: " ++ (show(mediaInfectados c)) ++ "\nmedia pessoas que coabitam: " ++ (show(mediaConvivio c)))
    putStrLn "As informacoes foram adicionadas ao arquivo covid.txt"

totalRespostas :: String -> Int
totalRespostas l = length(filter (/= "\n") (lines l))