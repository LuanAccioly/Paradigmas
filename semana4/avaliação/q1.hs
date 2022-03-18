{-
Uma lista é uma sublista de outra se os elementos da primeira ocorrem na segunda seguindo 
a mesma ordem. Por exemplo, "ship" é uma sublista de "fish and chips", mas não de "hippies".
Já uma lista é uma subsequência de outra se a primeira ocorre como uma sequência de elementos
dentro da outra. Por exemplo, "chip" é uma subsequência de "fish and chips", mas não de "chin up".
Defina duas funções, uma para verificar se uma string é sublista de outra
(sublista :: String -> String -> Bool), e a segunda para verificar se uma string é uma
subsequência de outra (subsequencia :: String -> String -> Bool).
-}

sublista :: String -> String -> Bool
sublista [] [] = False
sublista [] _ = False
sublista _ [] = False
sublista a b = if (elem a (listaDeSubsequencias b)) then True else False

subsequencia :: String -> String -> Bool
subsequencia [] [] = True
subsequencia [] _ = False
subsequencia _ [] = False
subsequencia a b = if (elem a (listaDeSubsequencias b)) then True else False

listaDeSubsequencias :: [a] -> [[a]]
listaDeSubsequencias xs =  [] : subsequenciasNaoVazias xs

subsequenciasNaoVazias :: [a] -> [[a]]
subsequenciasNaoVazias [] = []
subsequenciasNaoVazias (x:xs) = [x] : foldr f [] (subsequenciasNaoVazias xs)
  where f ys r = ys : (x : ys) : r

{-

Avaliação 1: Parcialmente correto. A função subsequencia não reconhece quando a sequencia é interrompida e retorna True quando deveria retornar False.

casos testados:
sublista     "abcd" "abcxd"           = True  / Correto.
sublista "chip" "fish and chips"      = True  / Correto.
sublista "abcd" "ab U dc"             = False / Correto. A 
subsequencia "chip" "fish and chips"  = True  / Correto.
subsequencia "abcd" "abcxd"           = True  / Incorreto. A sequencia foi interrompida pelo 'x'
subsequencia "chip" "chin up"         = True  / Incorreto. O conjunto foi interrompido pelo 'n u'
subsequencia "abcd" "ab U dc"         = False / Correto.





-}