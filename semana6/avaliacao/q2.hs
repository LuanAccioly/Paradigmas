{-
Filamentos de DNA e RNA são compostos por seus nucleotoides. 

Os quatro nucleotoides do DNA são: adenina (A), citosina (C), guanina (G) e timina (T).

Os quatro nucleotoides do RNA são: adenina (A), citosina (C), guanina (G) e uracil (U).

Um filamento de DNA pode ser transcrito para um de RNA substituindo cada nucleotoide pelo seu complemento:

G -> C
C -> G
T -> A
A -> U
Assim, crie um programa em haskell que lê um arquivo "dna.txt" cujas linhas correspondem a filamentos de DNA. 
Em seguida, crie um novo arquivo "rna.txt" com as transcrições para RNA dos filamentos fornecidos no arquivo 
de entrada. Abaixo segue um exemplo.

Dado um arquivo "dna.txt" com o seguinte conteúdo:

ACGTGGTCTTAA

CGTA

Seu programa deve gerar um arquivo "rna.txt" com o seguinte conteúdo:

UGCACCAGAAUU

GCAU

Linhas em branco devem gerar linhas em branco no arquivo de saída. Caso existam caracteres inválidos nas linhas 
do arquivo de entrada, a linha correspondente no arquivo de saída deve informar uma mensagem de erro 
"Filamento de DNA inválido!".
-}

{-
    Avaliação questão 2:
    
    Questão implementada perfeitamente.
    o colega utiliza funções auxiliares para "traduzir" os caracteres e gera dois arquivos (como solicitado), um contendo o dna e outro contendo o rna. 

    Casos de teste:
    ===========================
        
        DNA:
        --------------
        "ACGTGGTCTTAA"

        "CGTA"
        --------------
        RNA:
        --------------
        UGCACCAGAAUU

        GCAU
        --------------


        DNA:
        --------------
        "CGATCGATCGGTCAGCT"

        "TGCATGCGCTACG"
        --------------
        RNA:
        --------------
        GCUAGCUAGCCAGUCGA

        ACGUACGCGAUGC
        --------------

    ===========================    
    Em caso de passar um DNA vazio, o programa encerra.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

dnaToRna:: Char -> String
dnaToRna x | x == 'G' = "C"
        | x == 'C' = "G"
        | x == 'T' = "A"
        | x == 'A' = "U"       
        | x == '\n' = "\n"
        | otherwise = "Filamento de DNA invalido!"

funcao :: IO ()
funcao = do
    dna <- readFile "dna.txt"
    writeFile "rna.txt" (traduz (dna))

traduz :: [Char] -> [Char]
traduz [x] = dnaToRna x
traduz (x:xs) = dnaToRna x ++ traduz xs

--FUNCAO A SER USADA
main:: IO ()
main = do
    --MODIFIQUE AQUI O ARQUIVO DE ENTRADA
    writeFile "dna.txt" ""
    funcao
