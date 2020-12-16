module Utilitarios where

import Data.List
import Data.Function
import Data.Ord
import Text.Read

-- Funcao calcula a distancia euclidiana entre duas listas de doubles.
-- A funcao recebe as coordenadas/pontos e calcula a distancia euclidiana
-- Entrada: Lista de double, Lista de Double
-- Saida: Distancia euclidiana desses pontos
calculaDistancia :: [Double] -> [Double] -> Double
calculaDistancia coord1 coord2 = sqrt (calculaDistancia coord1 coord2)
                    where
                      calculaDistancia [] _ = 0
                      calculaDistancia _ [] = 0
                      calculaDistancia (x1:xs1) (x2:xs2) = (x1 - x2) ^ 2 + calculaDistancia xs1 xs2


-- Funcao recebe uma lista de tuplas de teste ja classificadas e retorna a porcentagem
-- de acertos entre a classe real e a classe "chutada"
-- Entrada: Lista de tuplas de teste classificadas
-- Saida: Double (porcentagem de acertos)
calculaAcuracia3 :: [((String,[Double]),String)] -> Double
calculaAcuracia3 xs = fromIntegral((length (filtro xs))*100)/fromIntegral(length xs)


-- Função que percorre cada fold e chama a função calculaAcuracia3 para
-- obter as acuracias individuais, então essa funcao faz o somatorio
-- e retorna esse valor somado
-- Entrada: Lista de Lista de tuplas ja classificadas
-- Saida: Somatório dos valores das acuracias de cada fold
calculaAcuracia2 :: [[((String,[Double]),String)]] -> Double
calculaAcuracia2 (x:[]) = calculaAcuracia3 x
calculaAcuracia2 (x:xs) = 0 + calculaAcuracia3 x + calculaAcuracia2 xs


-- Funcao que recebe da funcao calculaAcuraria2 os valores somados das 
-- acurácias de cada folds e divide pela quantidade de folds e obtemos
-- a média das acuracias
-- Entrada: Lista de Lista de tuplas ja classificadas
-- Saida: valor das acuracias medias de todos os folds
calculaAcuracia :: [[((String,[Double]),String)]] -> Double
calculaAcuracia xs = (calculaAcuracia2 xs / fromIntegral(length xs))


-- Funcao que monta uma lista de tuplas acordo com a classificacao, ou seja, a funcao
-- percorre a lista de tuplas de teste ja classificadas e compara se a sua real 
-- classe é igual a classe "chutada", se isso ocorre, é adicionada na lista.
-- Entrada Lista de tuplas de teste classificada
-- Saida: Lista de tuplas de teste classificada corretamente
filtro :: [((String,[Double]),String)] -> [((String,[Double]),String)]  
filtro xs = filter (\x -> (fst (fst x)) == (snd x)) xs


-- Funcao que monta uma lista de tuplas acordo com o nome da classe, ou seja,
-- a funcao percorre a lista de tuplas e compara se o nome da classe da 
-- enessima posicao é igual ao nome da classe do primeiro elemento dessa 
-- mesma lista e se for igual, é adicionada na lista.
-- Entrada Lista de tuplas de classe e coordenadas
-- Saida: Lista de tuplas de classe e coordenadas se for da mesma classe
filtro2 :: [(String,[Double])] -> [(String,[Double])]
filtro2 xs = filter (\x -> (fst x) == (fst (head xs))) xs


-- Funcao que monta uma lista de tuplas acordo com o nome da classe, ou seja,
-- a funcao percorre a lista de tuplas e compara se o nome da classe da 
-- enessima posicao é diferente ao nome da classe do primeiro elemento dessa 
-- mesma lista e se for diferente, é adicionada na lista.
-- Entrada Lista de tuplas de classe e coordenadas
-- Saida: Lista de tuplas de classe e coordenadas se não for da mesma classe
filtro3 :: [(String,[Double])] -> [(String,[Double])]
filtro3 xs = filter (\x -> (fst x) /= (fst (head xs))) xs


-- Funçao que agrupa separadamente em uma nova lista as classes que são iguais
-- Exemplo: juntaCoord [("1",[1.0,2.0,4.0]),("2",[1.0,1.0,1.0]),("1",[4.0,5.0,6.0])]
-- gera [[("1",[1.0,2.0,4.0]),("1",[4.0,5.0,6.0])],[("2",[1.0,1.0,1.0])]]
-- Entrada: Lista de tuplas de classes e coordenadas
-- Saida: Lista de lista de tuplas agrupadas pela classe
juntaCoord :: [(String,[Double])] -> [[(String,[Double])]]
juntaCoord xs = groupBy (\x y -> (fst x) == (fst y)) (ordenaClasse xs)


-- Funcao que junta os pontos da mesma classe em um uma lista de tuplas de classe
-- e lista de todos os pontos, ou seja, a funcao filtra todos os pontos de uma mesma
-- classe e agrupa em uma lista de tupla
-- Entrada: Lista de tuplas de treino
-- Saida: Lista de tuplas de treino com todos os pontos agrupados em uma lista
juntaClasses :: (Eq a, Ord a) => [[(a, b)]] -> [[(a, [b])]]
juntaClasses xs = [juntaClasses1 x | x <- xs]


-- Funcao que junta os pontos da mesma classe em um uma lista de tuplas de classe
-- e lista de todos os pontos, ou seja, a funcao filtra todos os pontos de uma mesma
-- classe e agrupa em uma lista de tupla
-- Entrada: Lista de tuplas de treino
-- Saida: Lista de tuplas de treino com todos os pontos agrupados em uma lista
juntaClasses1 :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
juntaClasses1 = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)


-- Funcao recebe uma lista de tuplas de teste ja classificadas e retorna a porcentagem
-- de acertos entre a classe real e a classe "chutada"
-- Entrada: Lista de tuplas de teste classificadas
-- Saida: Double (porcentagem de acertos)
calculaDesvio2 :: [((String,[Double]),String)] -> Double
calculaDesvio2 xs = fromIntegral((length (filtro xs))*100)/fromIntegral(length xs)


-- Funcao que percorre cada um dos fold e gera uma lista de double
-- para calculo do desvio padrão de cada fold
-- Entrada: Lista de lista de tuplas de teste classificadas
-- Saida: Lista de double 
calculaDesvio1 :: [[((String,[Double]),String)]] -> [Double]
calculaDesvio1 (x:[]) = [calculaDesvio2 x]
calculaDesvio1 (x:xs) = [calculaDesvio2 x] ++ calculaDesvio1 xs


-- Função que chama a funcao calculaDesvio1 para realizar as operações.
-- Entrada: Lista de lista de tuplas ja classificadas
-- Saida: desvio padrao de todos os folds
calculaDesvio :: [[((String,[Double]),String)]] -> Double
calculaDesvio xs = desvioPadrao (calculaDesvio1 xs)


-- Calcula o desvio padrao
-- Entrada: Lista de double
-- Saida: Desvio padrao
desvioPadrao :: [Double] -> Double
desvioPadrao xs = sqrt . average . map ((^2) . (-) axs) $ xs
           where average = (/) <$> sum <*> realToFrac . length
                 axs     = average xs


-- Calcula a média de uma lista de double
-- Entrada: lista de double
-- Saida: média da lista correspondente
media :: [Double] -> Double
media xs = sum xs / (fromIntegral (length xs))


-- Função que percorre os folds e chama a funcao centroideCalculo para realizar as
-- operações
-- Entrada: Lista de lista de tuplas de classe e a lista de todos os pontos 
-- dessa classe.
-- Saida: Lista de lista de tuplas de classe e os pontos calculados pelo centroide.
calculaCentroide :: [[(String,[[Double]])]] -> [[(String,[Double])]]
calculaCentroide xs = [centroideCalculo x | x <- xs]


-- Funcao que calcula o centroide (média) de todas as coordenadas (x1..xn) da lista de
-- todos os pontos agrupados de cada classe e retorna uma lista das classes e suas 
-- devidas coordenadas (x1..xn) calculadas.
-- Entrada: Lista de tuplas de classe e a lista de todos os pontos dessa classe.
-- Saida: Lista de tuplas de classe e os pontos calculados pelo centroide.
centroideCalculo :: [(String,[[Double]])] -> [(String,[Double])]
centroideCalculo xs = [(fst x, centroideUnica (length (snd x)) (centroide (snd x))) | x <- xs]


-- Funcao que divide cada elemento de uma lista de Double por uma valor inteiro,
-- ou seja, dada uma lista de double xs, cada elemento será dividida pelo Int tam
-- Entrada: Inteiro e Lista de double
-- Saida: Lista de double
centroideUnica :: Int -> [Double] -> [Double]
centroideUnica tam xs = map (/ fromIntegral(tam)) xs


-- Funcao que faz o somatorio das mesmas coordenadas de pontos de uma lista de lista
-- de double, por exemplo, centroide [[1,2,3],[1,3,4]] gera [2.0,5.0,7.0]
-- Entrada: Lista de lista de double
-- Saida: Lista de double
centroide :: [[Double]] -> [Double] 
centroide (x:[]) = x
centroide (x:xs) = zipWith (+) x $ centroide xs


-- Funcao que ordena a lista de tupla de classes e coordenadas pela classe
-- Entrada: Lista de tuplas de classes e coordenadas
-- Saida:  Lista de tuplas de classes e coordenadas ordenadas pela classe
ordenaClasse :: [(String,[Double])] -> [(String,[Double])]
ordenaClasse [] = []
ordenaClasse xs = (filtro2 xs) ++ ordenaClasse (filtro3 xs)


-- Função que ordena de acordo com a média das distancias de cada um dos folds
-- Ex: ordenaDist ("1",[1,1]) [[("1",[1,2]),("1",[2,1])],[("2",[1,2]),("2",[1,1])]]
-- gera [(0.5,"2"),(1.0,"1")]
-- Entrada: Tupla de classe e coordenadas (dado), lista de lista de tuplas de dados
-- Saida: Lista de tupla de distancia media e classe, ordenada.
ordenaDist :: (String,[Double]) -> [[(String,[Double])]] -> [(Double,String)]
ordenaDist z xs = sortBy (compare `on` fst) [ (media [calculaDistancia (snd z) (snd y) | y <- x], (fst (head x))) | x <- xs ]
