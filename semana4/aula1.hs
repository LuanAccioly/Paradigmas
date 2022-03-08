somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xy) = x + somaLista xy
--somaLista l = (head l) + (somaLista (tail l))

maiorElem :: [Int] -> Int
maiorElem [] = 0
maiorElem [x] = x
maiorElem (x:xs) | x > (maiorElem xs) = x  
                 | otherwise = maiorElem xs

dobraLista :: [Int] -> [Int]
--dobraLista [] = []
--dobraLista (x:xs) = (x * 2):(dobraLista xs)
dobraLista l = [2 * y | y <- l]

dobraListaPares l = [2 * y | y <- l, even y]

somaListaTupla :: [(Int, Int)] -> [Int]
somaListaTupla l = [x+y | (x,y) <- l]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]