module Formatacao where

import System.IO
import Data.List
import Numeric
import Utilitarios


-- Funcao que separa a lista pelo caractere passada (split)
-- Entrada: um caracter e uma lista (string)
-- Saida: uma lista de listas (strings) que foram separadas pelo caractere
-- Exemplo: separa ',' "oi,tudo,bom" gera ["oi","tudo","bom"]
separa :: (Eq a) => a -> [a] -> [[a]]
separa _ [] = []
separa c as = w : separa c r
    where 
        (w,r) = separa' c as []

        separa' _ [] l1 = (l1,[])
        separa' c (a:as) l1
            | a /= c = separa' c as (l1++[a])
            | otherwise = (l1,as)


-- Funcao de projeto que percorre toda a lista de listas de string que
-- corresponde a cada linha do arquivo de entrada e cria uma lista de 
-- tuplas da classe e dos pontos da classe sendo que o ultimo elemento
-- sempre é o nome da classe
-- Entrada: Uma lista de listas de strings
-- Saida: Uma lista de tuplas de String (nome da classe) e [Double] (pontos)
criaTupla :: [[String]] -> [(String,[Double])]
criaTupla xs = [ (ultimo x, coord x) | x <- xs ]
                where
                    coord = \x -> map (readDouble) (init x)
                    ultimo = last


-- Funcao que converte uma string(numérica) em Double
-- Entrada: string (numerica)
-- Saida: o numero double correspondente
readDouble :: String -> Double
readDouble = read


-- Função que recebe a lista de folds inteiros divididos e caso haja folds
-- desbalanceados, ele balanceia de acordo com a especificação do trabalho
-- Entrada: Lista de de inteiros (os folds)
-- Saida: Lista de lista de inteiros balanceada 
divideFolds2 :: Int -> Int -> [[Int]] -> [[Int]]
divideFolds2 f k xs = if k == 0 then xs
                    else if (length xs)-f > 1 then divideFolds2 f k (removedalista xs ++ agrupaLista (reverse xs))
                    else reverse(tail(reverse((divideFolds3 (take k xs) (last xs)) ++ reverse (take ((length xs)-k) (reverse xs)))))


-- Função que recebe uma lista de lista de inteiros e remove os dois
-- ultimos elementos dessa lista
-- Entrada: Lista de lista de inteiros
-- Saida: Lista de lista de inteiros sem os dois ultimos elementos
removedalista:: [[Int]] -> [[Int]]
removedalista xs = reverse (tail ( tail (reverse xs)))


-- Função que agrupa em uma lista os dois primeiros elementos
-- da lista de lista passados no parâmentro
-- Ex: agrupaLista [[1,2,3],[5,4],[6,7,8],[9,10,11],[12,13,14]]
-- gera [[5,4,3,2,1]]
-- Entrada: Lista de lista de inteiros
-- Saida: Lista de lista de inteiros agrupados os dois primeiros
-- elementos da lista passada
agrupaLista :: [[Int]] -> [[Int]]
agrupaLista (x:xs) = [reverse (x ++ reverse (head xs))]


-- Funcao que dada uma lista de lista de Inteiros e uma lista
-- de inteiros, retorna uma lista de lista com os n elementos
-- da lista de inteiros agrupados com os n primeiros elementos
-- da lista de lista de inteiros, por exemplo:
-- divideFolds3 [[1,2,3],[6,7,8],[9,10,11],[12,13,14]] [21,22]
-- gera [[1,2,3,21],[6,7,8,22]]
divideFolds3 :: [[Int]] -> [Int] -> [[Int]]
divideFolds3 xs xl = tuplaParaLista (zip xs xl)


-- Funcao que dada uma tupla de lista de inteiros com um inteiro
-- retorna uma lista de inteiros com o segundo elemento da tupla
-- agrupado na lista do primeiro elemento da tupla.
-- Ex: pairToList ([1,2,3],4) gera [1,2,3,4]
-- Entrada: tupla de lista de inteiro com inteiro
-- Saida: Lista de inteiro
tuplaParaLista2 :: ([Int], Int) -> [Int]
tuplaParaLista2 (x,y) = x ++ (geralista y)


-- Função que tranforma um numero em uma lista com esse numero
-- Entrada: Inteiro
-- Saida: Lista desse inteiro
geralista:: Int -> [Int]
geralista x = [x]


-- Função que percorre a lista de tuplas e chama a funcao
-- tuplaParaLista2 para realizar as operações
-- Ex: tuplaParaLista [([1,2,3],4),([5,6,7],8)]
-- gera [[1,2,3,4],[5,6,7,8]]
-- Entrada: lista de tuplas de lista de inteiros com inteiro
-- Saida, Lista de lista de inteiros agrupados para cada uma
-- das tuplas, os seus primeiro e segundo elemtento da tupla
tuplaParaLista :: [([Int],Int)] -> [[Int]]
tuplaParaLista = map tuplaParaLista2


-- Funcao que agrupa uma lista de inteiro de acordo com o int
-- passado, e gera uma lista de lista com esses grupos
-- Ex: divideFolds 2 [1,2,3,4,5,6,7,8]
-- gera [[1,2],[3,4],[5,6],[7,8]]
-- Ex2: divideFolds 2 [1,2,3,4,5,6,7]
-- gera [[1,2],[3,4],[5,6],[7]]
-- Entrada: número inteiro para representar a quantidade de
-- elementos agrupados e a lista de inteiros
-- Saida: Lista de lista agrupadas pelo valor de int passado
divideFolds :: Int -> [Int] -> [[Int]]
divideFolds n ls
    | n <= 0 || null ls = []
    | otherwise = (take n ls):(divideFolds n (drop n ls))


-- Funcao que dada uma lista de lista de inteiros e um inteiro
-- gera uma lista complementar aos elementos da lista de lista
-- passada, por exemplo:
-- listaSeparadaComplementar 5 [[1,2,3],[0,3,5]]
-- gera [[0,4,5],[1,2,4]]
-- Entrada: Inteiro e Lista de lista de inteiros
-- Saida: Lista de lista de inteiros complementar
listaSeparadaComplementar:: Int -> [[Int]] -> [[Int]]
listaSeparadaComplementar i xs = [[0..i] \\ x | x <- xs]


-- Funcao que recebe os dados de todos os folds e suas 
-- respectivas médias e desvios padrao e gera uma lista com 
-- esses dados padronizados.
-- Entrada: Lista de lista de dados, lista de lista de doubles
-- (medias) e lista de lista de double (devios)
-- Saida: Lista de lista de dados com as coodenadas padronizadas
-- pelo zscore
padronizaDados :: [[(String,[Double])]] -> [[Double]] -> [[Double]] -> [[(String,[Double])]]
padronizaDados (x:[]) (y:[]) (z:[]) = [padronizaDados2 x y z]
padronizaDados (x:xs) (y:ys) (z:zs) = [padronizaDados2 x y z] ++ padronizaDados xs ys zs


-- Funcao que recebe a lista de dados de cada fold e monta
-- a lista dos novos dados com coordenadas padronizadas para
-- cada um dos dados do fold
-- Entrada: Lista de tupla de classe e coordenadas, lista do
-- da media de cada coordenada, e lista do desvio de cada coor-
-- denada.
-- Saida: Lista de tuplas de classe e coordenadas padronizadas
padronizaDados2 :: [(String,[Double])] -> [Double] -> [Double] -> [(String,[Double])]
padronizaDados2 xs ys zs = [ (fst x, coordPadronizada (snd x) ys zs) | x <- xs]


-- Funcao que percorre cada uma das coordenadas e chama a funcao
-- zscore para retornar a lista padronizada
-- Entrada: Lista das coordenadas a ser padronizadas, lista da
-- média de cada uma dessas coordenadas e lista do desvio padrao
-- para cada uma dessas coordenadas
-- Saida: Lista das coordenadas padronizadas 
coordPadronizada :: [Double] -> [Double] -> [Double] -> [Double]
coordPadronizada (x:[]) (y:[]) (z:[]) = [zscore x y z]
coordPadronizada (x:xs) (y:ys) (z:zs) = [zscore x y z] ++ coordPadronizada xs ys zs


-- Funcao usada para padronizar os dados de acordo com a média
-- e o desvio padrao
-- Entrada: coordenada a ser padronizada, média e desvio padrao
-- Saida: Coordenada padronizada
zscore :: Double -> Double -> Double -> Double
zscore xs media desvio = (xs - media) / desvio 


-- Funcao que percorre cada um dos folds e chama a funcao
-- agrupaListaTreino2 para retornas as listas de lista
-- de todas as coordenadas, apos receber essas listas de cada
-- um dos folds é aplicada a funcao transpose para agrupar
-- os xi..xn elementos.
-- Ex: agrupaListaTreino [[("1",[1,2,3]),("2",[5,6,7])]]
-- gera [[[1.0,5.0],[2.0,6.0],[3.0,7.0]]]
-- Entrada: Lista de lista de tuplas de dados
-- Saida: Lista de lista de lista de double
agrupaListaTreino :: [[(String,[Double])]] -> [[[Double]]]
agrupaListaTreino xs = [ transpose (agrupaListaTreino2 x) | x <- xs]


-- Função que agrupa as coordenadas de cada lista de double 
-- e retorna uma lista de listas dessas coordenadas
-- Entrada: Lista de tuplas de classe e coordenadas
-- Saida: Lista de lista com todas as coordenadas do conjunto
agrupaListaTreino2 :: [(String,[Double])] -> [[Double]]
agrupaListaTreino2 (x:[]) = [(snd x)]
agrupaListaTreino2 (x:xs) = [(snd x)] ++ agrupaListaTreino2 xs


-- Funcao que percorre cada uma das listas de listas
-- de listas de double e chama a funcao mediaTreino2
-- para realizar as operacoes
-- Entrada: Lista de lista de lista de double
-- Saida: Lista de lista de double as médias
mediaTreino :: [[[Double]]] -> [[Double]]
mediaTreino xs = [(mediaTreino2 x) | x <- xs]


-- Funcao que percorre cada a lista de lista de double
-- e chama a média para calcular as médias das lista
-- Entrada: Lista de lista de double
-- Saida: Lista de double 
mediaTreino2 :: [[Double]] -> [Double]
mediaTreino2 xs = [ media x | x <- xs]


-- Funcao que percorre cada uma das listas de listas
-- de listas de double e chama a funcao desvioTreino2
-- para realizar as operacoes
-- Entrada: Lista de lista de lista de double
-- Saida: Lista de lista de double com os desvios
desvioTreino :: [[[Double]]] -> [[Double]]
desvioTreino xs = [(desvioTreino2 x) | x <- xs]


-- Funcao que percorre cada a lista de lista de double
-- e chama o desvio padrao para calcular o desvio da lista
-- Entrada: Lista de lista de double
-- Saida: Lista de double 
desvioTreino2 :: [[Double]] -> [Double]
desvioTreino2 xs = [ desvioPadrao x | x <- xs]