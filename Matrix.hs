module Matrix where


-- A funcao monta a matrix de confusao.
-- Essa funcao percorre a lista de strings de teste com classificacao verdadeira e
-- e a lista de strings de teste com a classificacao "chutada" e vai realizando
-- somas correspondente para cada classe para montar uma lista de lista de inteiros
-- com cada elemento dessa listas de inteiros é o somatorio das classes da matriz
-- de confusao.
-- Entrada: Lista de String de teste com classe verdadeira, Lista de String de 
-- teste com classe chutada e a lista de todas as classes.
-- Saida: lista de lista de inteiros, correspondente a matriz de confusao.
matrixContada2 :: [String] -> [String] -> [String] -> [[Int]]
matrixContada2 strVer strClass classes = [ [ realizasomas x y strVer strClass | x <- classes ] | y <- classes ]


-- Funçao que percorre cada um dos folds e chama a função matrixContada2 para realizar
-- as operações
-- Entrada: Lista de lista de Strings de classes verdadeiras e lista de lista de 
-- String de classes classificadas e as listas de classes existentes
-- Saida: Lista de Lista de Lista de inteiros correspondente as matrizes de 
-- confusao de cada um dos folds
matrixContada :: [[String]] -> [[String]] -> [String] -> [[[Int]]]
matrixContada (x:[]) (y:[]) zs = [matrixContada2 x y zs]
matrixContada (x:xs) (y:ys) zs = [matrixContada2 x y zs] ++ matrixContada xs ys zs


-- Funcao auxiliar que conta a quandade de mesma classe de acordo com a string s1 e s2,
-- ou seja, de acordo com s1 e s2 a funcao vai contar a quantidade de vezes que s1 e s2
-- sao iguais nas mesmas posicoes da matrix lstr e lste.
-- Exemplo: realizasomas "ver" "ver" ["ver","ver","vir"] ["ver","vir","ver"] gera 1
-- Exemplo 2: realizasomas "ver" "ver" ["ver","ver","vir"] ["ver","ver","ver"] gera 2
-- Entrada: String 1, String 2, Lista de String (lista de string treino), Lista de
-- String (lista de string teste)
-- Saida: Inteiro
realizasomas :: String -> String -> [String] -> [String] -> Int
realizasomas s1 s2 lstr lste = sum [ 1 | (s, o) <- (zip lstr lste), s1 == s, s2 == o]


-- Funcao que faz o somatorio das mesmas coordenadas de pontos de uma lista de lista
-- de int, por exemplo, centroide [[1,2,3],[1,3,4]] gera [2,5,7]
-- Entrada: Lista de lista de int
-- Saida: Lista de int
centroideInt :: [[Int]] -> [Int] 
centroideInt (x:[]) = x
centroideInt (x:xs) = zipWith (+) x $ centroideInt xs


-- Função que percorre cada fold para chamar a função centroide para realizar
-- as operações
-- Exemplo "matrixConfusao [[[1,2,3],[6,7,8]],[[4,5,6],[9,10,11]]]" gera
-- [[7,9,11],[13,15,17]]
-- Entrada: 
matrixConfusao :: [[[Int]]] -> [[Int]]
matrixConfusao xs = [ centroideInt x | x <- xs]


-- Função que junta cada xi..xn  de cada lista de inteiros dos folds e gera uma
-- lista com os indices xi, x2,..xn, agrupados
-- Exemplo: "juntaMatrizes [[[1,2,3],[4,5,6]],[[6,7,8],[9,10,11]]] 1" gera
-- [[[1,2,3],[6,7,8]],[[4,5,6],[9,10,11]]]
-- Entrada:  lista de inteiros de cada matriz de confusao de cada fold, quantidade
-- de classes existentes -1 (pois a posiçao na lista começa em 0)
-- Saída: lista de lista de lista de inteiros.
juntaMatrizes :: [[[Int]]] -> Int -> [[[Int]]]
juntaMatrizes xs xi = [[ x !! i |  x <- xs] | i <- [0..xi]]


-- Funcao que pega a matrix de confusao e gera uma string.
-- A funcao chama funcoes auxiliares para percorrer a lista de lista de inteiros e
-- vai passando cada lista e concatenando os resultados recursivamente.
-- Entrada: Lista de Lista de Int (matriz de confusao)
-- Saida: String em linha unica correspondente a matriz de confusao para impressao
imprimeMatrix :: [[Int]] -> String
imprimeMatrix (x:[]) = print_ x
imprimeMatrix (x:xs) = print_ x ++ imprimeMatrix xs


-- Funcao auxiliar que pega um Vetor de inteiros e gera uma string.
-- A funcao vai passando cada elemento da lista de int e vai chamando outra funcao
-- para transformar esse numero em string e vai concatenando recursivamente até
-- gerar uma lista de string concatenada.
-- Entrada: Lista de Int
-- Saida: String concatenada 
print_ :: [Int] -> String
print_ (x:[]) = printAux_ x ++ "\n"
print_ (x:xs) = printAux_ x ++ "," ++ print_ xs


-- Funcao que tranforma um inteiro em uma string sempre de tamanho 3.
-- Entrada: Inteiro a ser convertido
-- Saida: String de tamanho 3
printAux_ :: Int -> String
printAux_ x = reverse $ take 3 ((reverse $ (show x)) ++ [' ' | i <- [0..]]) 
