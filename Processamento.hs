module Processamento where

import Utilitarios
import Data.List
import Data.Function 
import Data.Ord


-- Essa funcao separa a lista de teste de acordo com os indices randomicos
-- Funcao de projeto que "separa" e monta uma lista de tuplas de classe 
-- e pontos do primeiro argumento de acordo com uma lista de Int que 
-- corresponde ao indice de cada elemento dessa lista do primeiro argumento.
-- Entrada: Lista de tupla, lista vazia (auxiliar), lista dos indices (deve
-- ter numeros correspondentes aos indices da lista de tupla)
-- Saida: Lista de tuplas correspondentes aos indices da lista de inteiros
criaLista2 :: [(String,[Double])] -> [(String,[Double])] -> [Int]-> [(String,[Double])]
criaLista2 _ xs [] = xs
criaLista2 ys xs as = criaLista2 ys (( ys !! (head as) ) : xs) (tail as)


-- Funçao que recebe os dados e a lista de folds e chama a função criaLista2
-- para criar as lista de tuplas correspondentes aos indices da lista de 
-- inteiros de cada um dos folds
-- Entrada: Lista de tuplas, lista de lista de inteiroos (folds)
-- Saida: Lista de lista de tuplas, ou seja, dados correspondentes aos
-- indices da lista de inteiros de cada um dos folds
criaLista :: [(String,[Double])] -> [[Int]] -> [[(String,[Double])]]
criaLista xs xa = [criaLista2 xs [] x | x <- xa]


-- A funcao percorre a lista de tuplas de teste gera uma lista de tuplas 
-- de teste e a classificacao correspondente.
-- Entrada: lista de tuplas de treino, lista de tuplas de teste
-- Saida: Lista de tuplas de tuplas de teste e sua classificacao
classificaClasse2 :: [(String,[Double])] -> [(String,[Double])] -> [((String,[Double]),String)]
classificaClasse2 xs ys = [ defineClasse z xs (calculaDistancia (snd z) (snd (head xs))) (fst (head xs)) | z <- ys]


-- Função que percorre cada posição n dos folds e chama a classificaClasse2
-- para realizar as operações de classificação, por fim, gera um lista da
-- classificação para cada um dos folds
-- Entrada: Lista de lista de tuplas de treino, lista de lista de tuplas de 
-- teste
-- Saida: Lista de lista de tuplas de tuplas de teste e sua classificacao
classificaClasse :: [[(String,[Double])]] -> [[(String,[Double])]] -> [[((String,[Double]),String)]]
classificaClasse (x:[]) (y:[]) = [classificaClasse2 x y]
classificaClasse (x:xs) (y:ys) = [classificaClasse2 x y] ++ classificaClasse xs ys


-- Funcao que precorre a lista de teste classifica uma tuplas de classe e pontos 
-- de acordo com a distancia de cada tupla da lista de treino
-- Entrada: tupla de classe e pontos, lista de tuplas de treino, Double (distancia)
-- e string (nome da classe de treinamento)
-- Saida: Tupla de tuplas de teste e sua classificacao
defineClasse :: (String,[Double]) -> [(String,[Double])] -> Double -> String -> ((String,[Double]),String)
defineClasse z [] _ nome = (z,nome)
defineClasse z (x:xs) dist nome = if calculaDistancia (snd z) (snd x) < dist 
                                  then defineClasse z xs (calculaDistancia (snd z) (snd x)) (fst x)
                                  else defineClasse z xs dist nome


-- Funcao que precorre a lista de teste classifica uma tuplas de classe e pontos 
-- de acordo com a distancia de cada tupla da lista de treino
-- Entrada: tupla de classe e coordenadas, lista de tuplas de treino, Double (distancia)
-- e tupla de classe e coordenadas 
-- Saida: Tupla de classe e coordenadas
defineClasse2 :: (String,[Double]) -> [(String,[Double])] -> Double -> (String,[Double]) -> (String,[Double])
defineClasse2 _ [] dist nome     = nome
defineClasse2 z (x:xs) dist nome = if calculaDistancia (snd z) (snd x) < dist
                                   then defineClasse2 z xs (calculaDistancia (snd z) (snd x)) x
                                   else defineClasse2 z xs dist nome


-- Funcao de projeto que gera uma lista de string de classes verdadeiras, ou seja,
-- a funcao pega do conjunto de teste a classe verdadeira.
-- Entrada: Lista de tuplas de teste ja classificadas
-- Saida: Lista de String de teste com classe verdadeira
armazenaNomeClasseVerdadeira2 :: [((String,[Double]),String)] -> [String]
armazenaNomeClasseVerdadeira2 xs = [ fst(fst x) | x <- xs]


-- Função que percorre os a lista de cada um dos folds e chama a função
-- armazenaNomeClasseVerdadeira2 para realizar as operações
-- Entrada: Lista de lista de tuplas de tuplas de teste e sua classificacao
-- Saida: Lista de lista de String de teste com classe verdadeira
armazenaNomeClasseVerdadeira :: [[((String,[Double]),String)]] -> [[String]]
armazenaNomeClasseVerdadeira xs = [ armazenaNomeClasseVerdadeira2 x | x <- xs]


-- Funcao de projeto que gera uma lista de string de classes "chutadas", ou seja,
-- a funcao pega do conjunto de teste a classe classificada pelos metodos do vizinho
-- e do centroide.
-- Entrada: Lista de tuplas de teste ja classificadas
-- Saida: Lista de String de teste com classe chutadas
armazenaNomeClasseClassificada2 :: [((String,[Double]),String)] -> [String]
armazenaNomeClasseClassificada2 xs = [ snd x | x <- xs]


-- Função que percorre os a lista de cada um dos folds e chama a função
-- armazenaNomeClasseClassificada2 para realizar as operações
-- Entrada: Lista de lista de tuplas de tuplas de teste e sua classificacao
-- Saida: Lista de lista de String de teste com classe "chutada"
armazenaNomeClasseClassificada :: [[((String,[Double]),String)]] -> [[String]]
armazenaNomeClasseClassificada xs = [ armazenaNomeClasseClassificada2 x | x <- xs]


-- Funcao que agrupa e ordena os k vizinhos mais próximos e gera uma lista 
-- de tuplas onde temos para cada dado os k-vizinhos mais proximos agrupados
-- pela mesma classe
-- Entrada: K-vizinhos mais próximos, Lista de treino, Lista de teste
-- Saida: Lista de tuplas de dado com lista de lista de lista de dados mais próximos
agrupaOrdenado :: Int -> [(String,[Double])] -> [(String,[Double])] -> [((String,[Double]),[[[(String,[Double])]]])]
agrupaOrdenado k xs ys = [ (y, groupBy ((==) `on` length) (reverse (sortBy (compare `on` length) (juntaCoord (pegaKVizinhos y xs k)) )) ) | y <- ys ]


-- Função que recebe um dado e classifica ele de acordo com a a função
-- ordenaDist, que retorna a lista de cada classe ordenada pela menor
-- média, então a funcao pega o primeiro elemento dessa lista e atribui
-- a classificação ao dado
-- Entrada: Tupla de classe e coordenada, Lista de Lista de tuplas de
-- classe e coordenadas (onde estão os k-vizinhos)
-- Saida: Tupla de dado e sua classifição correspondente
classificaClasseK2 :: (String,[Double]) -> [[(String,[Double])]] -> ((String,[Double]),String)
classificaClasseK2 x xs = ( x, snd (head (ordenaDist x xs)))


-- Função recebe o dado e a lista seus k-vizinhos mais próximos separados
-- em lista de acordo com a classe, se esta lista tiver mais de uma classe
-- então é chamada a função classificaClasseK2, caso contrario, a função
-- já classifica o dado
-- Entrada: Tupla de classe e coordenada, lista de lista de lista de dado
-- Saida: Tupla de dado de teste classificado
classificaClasseK :: (String,[Double]) -> [[[(String,[Double])]]] -> ((String,[Double]),String)
classificaClasseK z (x:[]) = (z, (fst (head (head x))))
classificaClasseK z (x:xs) = classificaClasseK2 z x


-- Funçao que percorre cada uma das listas tuplas dos dados e seus
-- respectivos k-vizinhos mais próximos e chama a funcao classificaClasseK
-- para realizar as operações
-- Entrada: Lista de tuplas dos dados e seus k-viznhos mais proximos
-- Saida: Lista de tuplas ja classificadas
classificaKVizinhos2 :: [((String,[Double]),[[[(String,[Double])]]])] -> [((String,[Double]),String)]
classificaKVizinhos2 xs = [ classificaClasseK (fst x) (snd x) | x <- xs]


-- Funcao que percorre cada posiçao n dos folds de treino e teste e
-- chama as função classificaKVizinhos para classsificar dada um dos
-- dados de cada um dos folds passados
-- Entrada: K-vizinhos requerido, lista lista de treino, lista de lista
-- de teste
-- Saida: Lista de cada um dos folds, de tuplas classificadas
classificaKVizinhos :: Int -> [[(String,[Double])]] -> [[(String,[Double])]] -> [[((String,[Double]),String)]]
classificaKVizinhos k (x:[]) (y:[]) = [classificaKVizinhos2 (agrupaOrdenado k x y)]
classificaKVizinhos k (x:xs) (y:ys) = [classificaKVizinhos2 (agrupaOrdenado k x y)] ++ classificaKVizinhos k xs ys


-- Funcao que adquire os k-vizinhos mais proximos de cada dado
-- Ex1: pegaKVizinhos ("1",[1,4]) [("1",[1,2]),("1",[2,1]),("2",[1,4])] 1
-- gera [("2",[1.0,4.0])]
-- Ex2: pegaKVizinhos ("1",[1,4]) [("1",[1,2]),("1",[2,1]),("2",[1,4])] 2
-- gera [("2",[1.0,4.0]),("1",[1.0,2.0])]
-- Entrada:  Tupla de classe e coordenadas (dado), Lista de dados, o valor
-- de k-vizinhos mais proximos
-- Saida: Lista de tamanho k, de tuplas de classe e coord dos vizinhos
-- mais próximos 
pegaKVizinhos :: (String,[Double]) -> [(String,[Double])] -> Int -> [(String,[Double])]
pegaKVizinhos _ _ 0  = []
pegaKVizinhos _ [] _ = []
pegaKVizinhos x xs k = (montaKVizinhos x xs) : pegaKVizinhos x (xs \\ [montaKVizinhos x xs]) (k-1)


-- Funcao que auxilia na montagem dos k-vizinhos mais proximos, pegando o 
-- dado mais proximo dentre a lista passada, de acordo com o dado passado
-- Exemplo: montaKVizinhos  ("1",[1,4]) [("1",[1,2]),("1",[2,1]),("2",[1,4])]
-- gera  ("2",[1.0,4.0])
-- Entrada: Tupla de classe e coordenadas, lista de lista de classes e coordenadas
-- Saida: Tupla de classe e coordenada mais próxima
montaKVizinhos :: (String,[Double]) -> [(String,[Double])] -> (String,[Double])
montaKVizinhos x xs = defineClasse2 x xs (calculaDistancia (snd x) (snd (head xs))) (head xs)




