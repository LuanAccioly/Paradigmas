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

{-
--Manipulação de arquivos
--readFile:: FilePath -> IO String
--writeFile:: FilePath -> String -> IO()
--appendFile:: FilePath -> String -> IO()

manipulaArquivo = do putStrLn "Digite o nome do arquivo"               
                     nomeArquivo <- getLine
                     putStrLn "Digite o conteudo do arquivo"
                     conteudoArquivo <- getLine
                     writeFile nomeArquivo conteudoArquivo
                     appendFile nomeArquivo "\n===Fim==="
                     c <- readFile nomeArquivo
                     putStrLn c
-}

--Função principal
{- relatorioCovid = do 
                    putStrLn "Digite seu nome"
                    nome <- getLine
                    putStrLn "Você está com confirmacao de diagnostico de infeccao por COVID - 19? R - 's' para sim e 'n' para nao."
                    confirmadoCovid <- getLine
                    putStrLn "Voce coabita com pessoas com diagnostico ou suspeita de infeccao por COVID-19? R - 's' para sim e 'n' para nao."
                    coabitaInfectados <- getLine
                    writeFile "covid.txt" nome
                    appendFile "covid.txt" (" " ++ confirmadoCovid ++ " " ++ coabitaInfectados)
                    relatorio <- readFile "covid.txt"
                    putStrLn relatorio -}

relatorio2 = do 
                
                putStrLn "Digite seu nome: "                    
                nome <- getLine
                if(nome /= "")
                    then do putStrLn "Você está com confirmacao de diagnostico de infeccao por COVID - 19? R - 's' para sim e 'n' para nao."
                            confirmadoCovid <- getLine
                            putStrLn "Voce coabita com pessoas com diagnostico ou suspeita de infeccao por COVID-19? R - 's' para sim e 'n' para nao."
                            coabitaInfectados <- getLine
                            putStrLn coabitaInfectados
                            appendFile "covid.txt" (nome ++ " " ++ confirmadoCovid ++ " " ++ coabitaInfectados ++ "\n")
                            relatorio <- readFile "covid.txt"
                            relatorio2

                else putStrLn "Verifique o arquivo covid.txt"   



