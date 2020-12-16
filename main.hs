-- Nome: Vinicius de Abreu Bozzi

import System.IO
import EntradaSaida
import Formatacao
import Random
import Processamento
import Utilitarios
import Matrix
import Data.List
import Numeric


main :: IO ()
main = do (arq_entrada, arq_saida, folds, kvizinhos, semente) <- entrada -- lê as informacoes da entrada do usuário
          arquivo_entrada <- readFile arq_entrada 
          let dados = criaTupla $ map (separa ',') $ lines arquivo_entrada -- dados armazena todos os dados, no formato listas de tuplas [(Classe,[Coordenadas])]
          let aleatorios = numAleatorios semente (length dados) -- recebe uma lista de Inteiros de números aleatórios [Int]
          let listaAleatorios = divideFolds (truncate(fromIntegral(length dados)/fromIntegral(folds))) aleatorios -- agrupa a lista de inteiros [Int] em tamanhos iguais ao parametro passado e armazena em uma lista [[Int]]
          let listaseparada = divideFolds2 folds (mod (length dados) folds) listaAleatorios -- garante que os aleatorios ficarão igual ao exposto na descrição do trabalho -> [[Int]]
          let dadosTreinoOri = criaLista dados (listaSeparadaComplementar ((length dados)-1) listaseparada) -- gera a lista de treino de acordo com o complemento da lista de aleatórios, no formato [[(String,[Coordenadas])]]
          let agrupaCoord_Treino = agrupaListaTreino dadosTreinoOri -- gera uma lista [[[Double]]] com cada uma das xi..xn coordenadas agrupadas para cada um dos folds de treino
          let media_treino = mediaTreino agrupaCoord_Treino -- gera uma lista [[Double]] com a média das xi..xn coordenadas de cada um dos folds de treino
          let desvio_treino = desvioTreino agrupaCoord_Treino -- gera uma lista [[Double]] com o desvio padrao das xi..xn coordenadas de cada um dos folds de treino
          let dadosTesteOri = criaLista dados listaseparada -- gera a lista de teste de acordo com a lista de aleatórios, no formato [[(String,[Coordenadas])]]
          let dadosTestePad = padronizaDados dadosTesteOri media_treino desvio_treino -- gera a lista de teste de acordo a padronização dos dados de teste, no formato [[(String,[Coordenadas])]]
          let dadosTreinoPad = padronizaDados dadosTreinoOri media_treino desvio_treino -- gera a lista de treino de acordo a padronização dos dados de treino, no formato [[(String,[Coordenadas])]]
          let classes = nub (map fst(dados)) -- função que extrai classes existentes dos dados lidos e gera uma lista [Classes]

          -- Vizinho
          let vizinho = classificaClasse dadosTreinoPad dadosTestePad -- cria lista de tuplas da classificacao de acordo com o vizinho mais próximo de acodo com cada um dos folds, no formato [[((Classe,[Coordenadas]),ClasseChutada)]]
          let acuracia_Vizinho = calculaAcuracia vizinho -- valor da acurácia média dos folds pelo método do vizinho mais proximo 
          putStrLn("Acuracia(vizinho): " ++ (showGFloat (Just 2) acuracia_Vizinho "%")) 
          let desvio_Vizinho = calculaDesvio vizinho -- valor da desvio padrao médio dos folds do vizinho mais proximo 
          putStrLn("Desvio-Padrao(vizinho): " ++ (showGFloat (Just 2) desvio_Vizinho "%"))
          let armazena1 = armazenaNomeClasseVerdadeira vizinho -- armazena a lista de lista de string de classes que sao verdadeiras de cada um dos folds do metodo do vizinho mais proximo
          let armazena2 = armazenaNomeClasseClassificada vizinho -- armazena a lista de lista de string de classes que sao chutadas, de cada um dos folds do metodo do vizinho mais proximo
          let matrix_Vizinho = matrixConfusao (juntaMatrizes (matrixContada armazena1 armazena2 classes) ((length classes)-1)) -- gera a matriz de confusao do metodo do vizinho mais proximo, no formato [[Int]]
          let impressao_Matriz_Vizinho = imprimeMatrix matrix_Vizinho -- gera uma string da matriz de confusao dos vizinho mais proximo, formatada para impressao 
          

          --Centroide
          let agrupaCentroide = calculaCentroide (juntaClasses dadosTreinoPad) -- cria uma lista de lista de tuplas de cada classe com seus pontos(centroides) para cada um dos folds, no formato [[(Classe,[Centroide])]] 
          let centroide = classificaClasse agrupaCentroide dadosTestePad -- cria uma lista de lista lista de tuplas da classificacao de acordo com o distancia euclidiana de cada um dos folds, no formato [[((Classe,[Centroide]),ClasseChutada)]]
          let acuracia_Centroide = calculaAcuracia centroide -- valor da acurácia média dos folds pelo método dos centroides 
          putStrLn("Acuracia(centroide): " ++ (showGFloat (Just 2) acuracia_Centroide "%")) 
          let desvio_Centroide = calculaDesvio centroide -- valor da desvio padrao médio dos folds do método dos centroides 
          putStrLn("Desvio-Padrao(centroide): " ++ (showGFloat (Just 2) desvio_Centroide "%"))
          let armazena3 = armazenaNomeClasseVerdadeira centroide -- armazena a lista de lista de string de classes que sao verdadeiras de cada um dos folds pelo método dos centroides
          let armazena4 = armazenaNomeClasseClassificada centroide -- armazena a lista de lista de string de classes que sao chutadas, de cada um dos folds pelo método dos centroides
          let matrix_Centroide = matrixConfusao (juntaMatrizes (matrixContada armazena3 armazena4 classes) ((length classes)-1)) -- gera a matriz de confusao do metodo do centroide, no formato [[Int]]
          let impressao_Matriz_Centroide = imprimeMatrix matrix_Centroide -- gera uma string da matriz de confusao do centroide, formatada para impressao 

          
          -- K-Vizinhos
          let k_vizinho = classificaKVizinhos kvizinhos dadosTreinoPad dadosTestePad -- cria lista de tuplas da classificacao de acordo com o k-vizinhos mais próximos de acodo com cada um dos folds, no formato [[((Classe,[Coordenadas]),ClasseChutada)]]
          let acuracia_KVizinho = calculaAcuracia k_vizinho -- valor da acurácia média dos folds pelo método do k-vizinhos mais proximos 
          putStrLn("Acuracia(k-vizinhos): " ++ (showGFloat (Just 2) acuracia_KVizinho "%")) 
          let desvio_KVizinho = calculaDesvio k_vizinho  -- valor da desvio padrao médio dos folds dos k-vizinhos mais proximos 
          putStrLn("Desvio-Padrao(k-vizinhos): " ++ (showGFloat (Just 2) desvio_KVizinho "%"))
          let armazena5 = armazenaNomeClasseVerdadeira k_vizinho -- armazena a lista de lista de string de classes que sao verdadeiras de cada um dos folds do metodo dos k-vizinhos mais proximos
          let armazena6 = armazenaNomeClasseClassificada k_vizinho -- armazena a lista de lista de string de classes que sao chutadas, de cada um dos folds do metodo dos k-vizinhos mais proximos
          let matrix_KVizinhos = matrixConfusao (juntaMatrizes (matrixContada armazena5 armazena6 classes) ((length classes)-1)) -- gera a matriz de confusao do metodo dos k-vizinhos mais proximos, no formato [[Int]]
          let impressao_Matriz_KVizinhos = imprimeMatrix matrix_KVizinhos -- gera uma string da matriz de confusao dos k-vizinhos mais proximos, formatada para impressao 

          
          saida arq_saida impressao_Matriz_Vizinho impressao_Matriz_Centroide impressao_Matriz_KVizinhos -- passa os dados das matrizes de confusao para escrita no arquivo de saida
          
          return()

          

