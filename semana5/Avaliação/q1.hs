{-

QUESTÃO:
    Blockchain é basicamente um banco de dados distribuído onde os dados são armazenados em blocos onde cada bloco possui transações financeiras e uma referência para o próximo bloco formando uma corrente. Assumindo os tipos de dados abaixo, crie uma função em Haskell que dado um Bloco e uma conta, retorna uma lista contendo os índices dos blocos que possuem transações (origem ou destino) com esta conta considerando todas as transações da cadeia de blocos.

USO: getIndices "<nome_da_conta>" sampleBlockchain

EXEMPLO: getIndices "vermelho" sampleBlockchain

-}

{-
Função implementada corretamente.
Casos testados:

getIndices "vermelho" sampleBlockchain = [0,2]      | Ok
getIndices "azul" sampleBlockchain     = [0,1,2]    | Ok
getIndices "" sampleBlockchain         = []         | Ok
getIndices "lavanda" sampleBlockchain  = []         | Ok 

-}

import Data.List
import Data.Maybe

---------- DATA TYPES ----------

data Transaction = Transaction { from :: String  -- conta que paga
                                ,to :: String    -- conta que recebe
                                ,value :: Float
                               } deriving (Show)

data Block = Block { index :: Int
                    ,transactions :: [Transaction]
                    ,next :: Maybe Block
                   } deriving (Show)

---------- SAMPLES ----------

-- Creates a sample blockchain for testing purposes.
sampleBlockchain :: Block
sampleBlockchain = first
    where first = Block 0 tlist0 second
          tlist0 = [t0,t1,t2]
          t0 = Transaction "azul" "rosa" 419
          t1 = Transaction "verde" "rosa" 39
          t2 = Transaction "vermelho" "rosa" 640

          second = Just (Block 1 tlist1 third)
          tlist1 = [t3,t4,t5]
          t3 = Transaction "azul" "laranja" 344
          t4 = Transaction "verde" "laranja" 624
          t5 = Transaction "amarelo" "laranja" 405

          third = Just (Block 2 tlist2 fourth)
          tlist2 = [t6,t7,t8]          
          t6 = Transaction "azul" "roxo" 262
          t7 = Transaction "vermelho" "roxo" 814
          t8 = Transaction "amarelo" "roxo" 91

          fourth = Just (Block 3 tlist3 fifth)
          tlist3 = [t9,t10]
          t9 = Transaction "preto" "branco" 999
          t10 = Transaction "branco" "preto" 420

          fifth = Just (Block 4 [] Nothing)

---------- FUNCTIONS ----------

-- Returns the accounts in a transaction.
accounts :: Transaction -> [String]
accounts t = [from t, to t]

-- Takes a account and a transaction and tells if the account is in the transaction.
inTransaction :: String -> Transaction -> Bool
inTransaction account transaction = account `elem` (accounts transaction)

-- Takes a account and a block and tells if the account is in the block transactions. 
inBlock :: String -> Block -> Bool
inBlock account block = isJust $ find (inTransaction account) (transactions block)

---------- FINAL ANSWER ----------

-- Takes a account and a blockchain and returns the indices of the blocks that contain that account. (Recursive)
getBlocksInChain :: String -> Maybe Block -> [Int]
getBlocksInChain _ Nothing = []
getBlocksInChain account b = if (account `inBlock` block)
                             then (index block) : nextBlock
                             else nextBlock
                             where block = fromJust b
                                   nextBlock = getBlocksInChain account (next block)

-- Uses the Just function on the input of the above function.
getIndices :: String -> Block -> [Int]
getIndices account block = getBlocksInChain account (Just block)

---------- USING LISTS ----------

-- Extracts all elements out of a Just list.
fromJustList :: [Maybe a] -> [a]
fromJustList list = map fromJust list

-- Turns the blockchain into a blocklist. (Recursive)
toBlocklist :: Maybe Block -> [Maybe Block]
toBlocklist Nothing = []
toBlocklist b = b : toBlocklist (maybe Nothing next b)

-- Creates a sample blocklist for testing purposes.
sampleBlocklist :: [Block]
sampleBlocklist = fromJustList $ toBlocklist $ Just sampleBlockchain

-- Takes a account and a blocklist and returns the indices of the blocks that contain that account.
getBlocksInList :: String -> [Block] -> [Int]
getBlocksInList account blist = [index b | b <- blist, account `inBlock` b]