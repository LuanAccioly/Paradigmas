escreverSaidaPadrao :: String -> IO ()
escreverSaidaPadrao x = putStrLn x

escrever4Vezes :: String -> IO ()
escrever4Vezes x = do   putStrLn x
                        putStrLn x
                        putStrLn x
                        putStrLn x

leituraEscrita = do x <- getLine
                    putStrLn ("Frase digitada: " ++ x)  


interacao = do putStrLn "Digite seu nome: "
               nome <- getLine
               putStrLn ("Seu nome é: " ++ nome)

--Manipulação de arquivos
--readFile:: FilePath -> IO String
--writeFile:: FilePath -> String -> IO()
--appendFile:: FilePath -> String -> IO()

manipulaArquivo = do putStrLn "Digite o nome do arquivo"               
                     nomeArquivo <- getLine
                     putStrLn "Digite o conteudo do arquivo"
                     conteudoArquivo <- getLine
                     writeFile "/textos/texto.txt" conteudoArquivo
                     appendFile nomeArquivo "\n===Fim==="
                     c <- readFile nomeArquivo
                     putStrLn c           