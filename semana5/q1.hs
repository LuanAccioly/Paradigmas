{-
    Blockchain é basicamente um banco de dados distribuído onde os dados são armazenados em blocos onde cada bloco possui transações financeiras e uma referência para o próximo bloco formando uma corrente. Assumindo os tipos de dados abaixo, crie uma função em Haskell que dado um Bloco como entrada, retorna o índice do bloco da corrente com o maior volume financeiro de transações;

    data Transacao = Transacao { de :: String -- Conta que paga

                                                    , para :: String -- Conta que recebe

                                                    , valor :: Float -- Quanto está pagando

                                                    } deriving (Show)

    data Bloco = Bloco { indice :: Int -- Indice do bloco

                                    , trs :: [Transacao] -- Lista de transações de um bloco

                                    , proximo :: Maybe Bloco -- Proximo bloco

                                    } deriving (Show)

    OBS: para definir um valor do tipo bloco é só especificar (Bloco ind trs prox), onde ind é um inteiro, trs é uma lista de transações e prox é um Maybe Bloco. O mesmo se aplica ao tipo Transacao.
-}



