data Estacao = Inverno | Verao | Primavera | Outono
data Temperatura = Frio | Quente deriving (Show)

clima :: Estacao -> Temperatura
clima Inverno = Frio
clima _ = Quente

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

mostraPessoa :: Pessoas -> String
mostraPessoa (Pessoa n i) = "Nome: " ++ n ++",Idade: " ++ (show i)

data Forma = Circulo Float | Retangulo Float Float | Ponto Int Int

ehRedondo :: Forma -> Bool
ehRedondo (Circulo _) = True
ehRedondo _ = False

area :: Forma -> Float
area (Retangulo b h) = b * h
area (Circulo r) = pi * r^2
area (Ponto _ _) = 0.0

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
avalia:: Expr -> Int
avalia (Lit x) = x
avalia (Add x y) = avalia(x) + avalia(y) 
avalia (Sub x y) = avalia(x) - avalia(y) 

 