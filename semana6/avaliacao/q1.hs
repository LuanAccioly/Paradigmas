{-
Crie um programa em Haskell que lê informações do teclado e ao final compila as informações recebidas como 
entrada em um arquivo de texto. Seu programa deverá repetidamente fazer as seguintes perguntas:

1 - Informe seu nome; R - string com o nome

2 - Você está com confirmação de diagnóstico de infecção por COVID - 19? R - 's' para sim e 'n' para não.

3 - Você coabita com pessoas com diagnóstico ou suspeita de infecção por COVID-19? R - 's' para sim e 'n' para não.

Seu programa deve repetir o processo de perguntas até que a string vazia seja informada na pergunta 1. Em seguida, 
seu programa deve gerar um arquivo "covid.txt" onde cada linha deve ter as informações de cada pessoa que 
respondeu e após listar as pessoas, as 3 últimas linhas devem ter as seguintes informações:

- totais de pessoas que responderam o questionário

- média de pessoas com confirmação de diagnóstico de infecção por COVID - 19

- média de pessoas que coabitam com pessoas com diagnóstico ou suspeita de infecção por COVID-19.
-}

{-
    Avaliação questão 1:

    O arquivo foi enviando contendo strings que utilizam de caracteres não reconhecidos pelo compilador e a função principal quebra logo na segunda pergunta, imagino que o aluno tenha deixado pra acrescentar essas strings por último e acabou não testando depois.

    Corrigindo as strings a função funciona bem. Cumpre os requisitos de repetir até que um nome de vacina seja passado como uma string vazia e recebe bem os valores pedidos. Usa condicionais pra verificar se os caracteres passados coindicem com os caracteres de um contaminado ou de um coabitante e se coincidir, chama novamente a função incrementando os seus valores inteiros respectivos.

    Caso testado:
    ==================================================================
    Walter White
    s
    n

    Jesse Pinkman
    n
    s

    Gus Frieng
    n
    n

    Jon Snow
    n
    s

    Tyrion Lannister
    s
    n

    Brandon Stark
    n
    s

    Arya Stark
    s
    s 

    Total de pessoas que responderam: 7
    M�dia pessoas com confirmacao de diagnostico de infeccao por COVID-19: 3/7
    M�dia pessoas que coabitam com pessoas com diagnostico ou suspeita de infeccao por COVID-19: 4/7
    ===================================================================
    
    Não sei se o valor de média retornado em forma de fração seria o desejável para o caso, mas de toda forma a conta está correta, assim como o retorno.
-}
interacao:: Int->Int->Int->IO ()
interacao x y z = do
    putStrLn "Informe seu nome;"
    nome <- getLine 
    if nome /= ""
        then do
            appendFile "covid.txt" (nome ++ "\n")

            putStrLn "Você está com confirmacao de diagnostico de infeccao por COVID - 19? R - 's' para sim e 'n' para nao."
            conf <- getLine
            appendFile "covid.txt" (conf ++ "\n")

            putStrLn "Voce coabita com pessoas com diagnostico ou suspeita de infeccao por COVID-19? R - 's' para sim e 'n' para nao."
            coab <- getLine 
            appendFile "covid.txt" (coab ++ "\n" ++ "\n")

            if conf == "s" && coab == "s"
                then interacao (x+1) (y+1) (z+1)
                else if conf == "s"
                    then interacao (x+1) (y+1) z
                    else if coab == "s"
                        then interacao (x+1) y (z+1)
                        else interacao (x+1) y z
        else do
            putStrLn "Finalizado"
            appendFile "covid.txt" ("Total de pessoas que responderam: " ++ (show x) ++ "\n")
            appendFile "covid.txt" ("Média pessoas com confirmacao de diagnostico de infeccao por COVID-19: " ++ (show y) ++ "/" ++ (show x) ++ "\n")
            appendFile "covid.txt" ("Média pessoas que coabitam com pessoas com diagnostico ou suspeita de infeccao por COVID-19: " ++ (show z) ++ "/" ++ (show x) ++ "\n")

--FUNCAO A SER UTILIZADA
main :: IO ()
main = do
    writeFile "covid.txt" ""
    interacao 0 0 0
