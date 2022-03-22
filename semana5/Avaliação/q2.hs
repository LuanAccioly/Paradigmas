{-

QUESTÃO:
    Um abrigo de animais disponibiliza gatos e cachorros para serem adotados. Eles precisam disponibilizar as informações sobre estes animais para possíveis interessados, gatos possuem raça, cor e idade, cachorros possuem raça, cor, idade e tamanho (pequeno, médio e grande). Crie um tipo de dado algébrico Pet para representar gatos e cachorros do abrigo. Em seguida crie uma função que dada uma lista de Pets e dois argumentos um para representar a raça e outro a idade, retorna a lista com todos os animais cuja a raça seja igual a passada como entrada e tenham idade menor ou igual a passada como parâmetro.

USO: getPets samplePetlist "<nome_da_raça>" <idade>

EXEMPLO: getPets samplePetlist "poodle" 7

-}

---------- DATA TYPES ----------

data Size = Small | Medium | Big deriving(Show)

data Pet = Cat { breed :: String
               , color :: String
               , age :: Int
               }
               |
           Dog { breed :: String
               , color :: String
               , age :: Int
               , size :: Size
               } deriving (Show)

---------- SAMPLES ----------

-- Creates a sample pet list for test purposes.
samplePetlist :: [Pet]
samplePetlist = [ Cat "sphynx" "white" 1
                , Cat "bengal" "spotted" 5
                , Cat "bengal" "marbled" 5
                , Dog "poodle" "brown" 7 Small
                , Dog "poodle" "black" 7 Small
                , Dog "poodle" "black" 8 Small
                , Dog "german shepherd" "black" 8 Big
                , Dog "border collie" "black and white" 10 Medium
                ]

---------- FINAL ANSWER ----------

getPets :: [Pet] -> String -> Int -> [Pet]
getPets list pBreed pAge = [ pet | pet <- list, breed pet == pBreed, age pet <= pAge]

{-
Casos testados baseado na lista de pets já criada: 
getPets samplePetlist "poodle" 7                | OK | Retorna os dois poodle com idade <= 7
getPets samplePetlist "border collie" 10        | Ok | Retorna o único border collie da lista
getPets samplePetlist "bengal" 6                | Ok | Retorna os dois bengal da lista
getPets samplePetlist "vira lata caramelo" 4    | Ok | Nenhum vira lata caramelo
getPets samplePetlist "" 0                      | Ok | Retorna uma lista vazia

-}