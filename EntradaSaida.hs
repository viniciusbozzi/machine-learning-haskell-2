module EntradaSaida where


entrada :: IO (String, String, Int, Int, Int)
entrada = do putStrLn "Forneca o nome do arquivo de entrada: "
             arquivo_entrada <- getLine
             putStrLn "Forneca o nome do arquivo de saida: "
             arquivo_saida <- getLine
             putStrLn "Forneca o número de folds: "
             foldsS <- getLine
             putStrLn "Forneca o número de vizinhos: "
             vizinhosS <- getLine
             putStrLn "Forneca o valor da semente para geracao randomizada: "
             sementeS <- getLine
             return (arquivo_entrada, arquivo_saida, readInt foldsS, readInt vizinhosS, readInt sementeS)


-- Funcao que converte uma string(numérica) em Int
-- Entrada: string (numerica)
-- Saida: o numero int correspondente
readInt :: String -> Int
readInt = read


saida :: String -> String -> String -> String -> IO ()
saida arq mv mc mkv = do writeFile arq ( "vizinho mais próximo:" ++ "\n" ++ mv ++ "\n"
                                         ++ "centroides:" ++ "\n" ++ mc ++ "\n" 
                                         ++ "k-vizinhos mais próximos:" ++ "\n" ++ mkv)