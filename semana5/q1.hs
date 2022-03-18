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

data Transacao = Transacao String String Float deriving (Show)
data Bloco = Bloco Int [Transacao] (Maybe Bloco) deriving (Show)

--Função principal
indiceVolume :: Bloco -> Int
indiceVolume a = (verificador a a)

mostraBloco:: Bloco -> Bloco -> Int
mostraBloco (Bloco x y (Just(z))) (Bloco a b (Nothing)) | (volume y) >= (volume b) = x
                                                        | otherwise = a                                                        
mostraBloco (Bloco x y (Just(z))) (Bloco a b (Just(c))) | (volume y) >= (volume b) = x 
                                                        | otherwise = a

volume :: [Transacao] -> Float
volume [] = 0.0
volume (x:xs) = (pegaValor x) + (volume xs)

pegaValor :: Transacao -> Float
pegaValor (Transacao x y z) = z

valor (Just x) = x

verificador :: Bloco -> Bloco -> Int
verificador x (Bloco a b (Nothing)) = mostraBloco x (Bloco a b (Nothing))
verificador x (Bloco a b (Just (c)))  | (mostraBloco x (Bloco a b (Just (c)))) == a = verificador (Bloco a b (Just (c))) c
                                                           | otherwise = verificador x c


mostraBloco (Bloco 1 [(Transacao "123" "321" 54.5)] (Just ((Bloco 2 [(Transacao "123" "321" 100.0)] Nothing)))) (Bloco 2 [(Transacao "123" "321" 1.0)] Nothing)


