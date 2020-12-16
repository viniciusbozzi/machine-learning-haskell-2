module Random where

import System.IO
import System.Random


-- funcao disponibilizada pelo professor
tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs

-- funcao disponibilizada pelo professor
removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)


-- Função que gera uma lista de numeros aleatórios de acordo com a semente e o tamanho passado
-- Entrada:  semente e tamanho 
-- Saida:  Lista de inteiros de [0...tamanho-1] dispostos aleatóriamente
numAleatorios :: Int -> Int -> [Int]
numAleatorios semente tamanho = take (tamanho) (removeDup ((randomRs (0, tamanho-1) (mkStdGen semente) :: [Int])))