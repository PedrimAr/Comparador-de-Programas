{-
O Makefile só funciona em Linux ou MAC. Se tiver WSL instalado, só digitar "make" no terminal que vai compilar e executar. 
Se tiver Git instalado, só digitar "mingw32-make" no terminal que também vai. 
Senão digita ghc -O2 -o similaridade main.hs para compilar e ./similaridade res.txt sep.txt c1.txt c2.txt para executar. 
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Text.Printf (printf)

-- Recebe os separadores e o texto sujo, e retorna o texto com espaços no lugar dos separadores 
limparTexto :: String -> String -> String
limparTexto sep texto = map verificarCaractere texto 
  where
    verificarCaractere :: Char -> Char
    verificarCaractere c = 
        if c `elem` sep 
            then ' '  -- Se o caractere 'c' estiver na lista 'sep', vira espaço 
            else c    -- Caso contrário, mantém o caractere original 

-- Recebe os separadores e o texto sujo, e retorna a lista de palavras prontas 
tokenize :: String -> String -> [String]
tokenize sep texto = 
    let textoLimpo = limparTexto sep texto 
    in words textoLimpo 

-- Constrói o mapa de frequências aplicando o peso dobrado para palavras reservadas 
constroiMapaDeFrequencias :: [String] -> [String] -> Map.Map String Int
constroiMapaDeFrequencias reservadas palavrasCod = foldl adicionarPalavra Map.empty palavrasCod 
  where
    adicionarPalavra :: Map.Map String Int -> String -> Map.Map String Int
    adicionarPalavra mapaAtual palavra =
        let peso = if palavra `elem` reservadas then 2 else 1 
        in Map.insertWith (+) palavra peso mapaAtual 

-- Ordena o relatório: frequências decrescentes e desempate lexicográfico (alfabético) crescente
ordenaFrequencias :: Map.Map String Int -> [(String, Int)]
ordenaFrequencias mapa = sortBy regraDeOrdenacao (Map.toList mapa)
  where
    regraDeOrdenacao (palavraA, freqA) (palavraB, freqB) =
        case compare freqB freqA of
            EQ -> compare palavraA palavraB  -- Se as frequências forem iguais, desempata alfabeticamente
            outro -> outro                   -- Senão, mantém a ordem decrescente de frequência

-- Calcula o valor de M aplicando a regra de até 10% de diferença em relação a f1
calculaM :: Map.Map String Int -> Map.Map String Int -> Double
calculaM map1 map2 = Map.foldrWithKey acumulaM 0.0 map1
  where
    acumulaM palavra f1 acc =
        let f2 = Map.findWithDefault 0 palavra map2 -- Se a palavra não existir em c2, f2 é 0
            f1Double = fromIntegral f1
            f2Double = fromIntegral f2
            limite = 0.1 * f1Double
            diferenca = abs (f1Double - f2Double)
        in if diferenca <= limite
           then acc + f1Double   -- Se estiver na margem de 10%, soma f1 ao acumulador m
           else acc

-- Calcula o índice de similaridade final (m / soma(f1))
calculaSimilaridade :: Map.Map String Int -> Map.Map String Int -> Double
calculaSimilaridade map1 map2 =
    let m = calculaM map1 map2
        somaF1 = fromIntegral (sum (Map.elems map1))
    in if somaF1 == 0 
       then 0.0 
       else m / somaF1

-- Ponto de entrada do programa 
main :: IO ()
main = do
    args <- getArgs 
    
    if length args /= 4 
        then do
            putStrLn "Erro: Número incorreto de argumentos." 
            putStrLn "Uso correto: ./similaridade <arquivo_res> <arquivo_sep> <arquivo_c1> <arquivo_c2>" 
            exitFailure 
        else do
            let [arqRes, arqSep, arqC1, arqC2] = args 
            
            strRes <- readFile arqRes 
            strSep <- readFile arqSep 
            strC1 <- readFile arqC1 
            strC2 <- readFile arqC2 
            
            let palavrasRes = words strRes 
            let palavrasC1 = tokenize strSep strC1 
            let palavrasC2 = tokenize strSep strC2 
            
            -- Geração dos mapas de frequência 
            let map1 = constroiMapaDeFrequencias palavrasRes palavrasC1 
            let map2 = constroiMapaDeFrequencias palavrasRes palavrasC2 
            
            -- 1. SAÍDA: Relatório com as frequências decrescentes de c1
            putStrLn "=== RELATÓRIO DE FREQUÊNCIAS (CÓDIGO 1) ==="
            let relatorioOrdenado = ordenaFrequencias map1
            mapM_ (\(palavra, freq) -> putStrLn $ palavra ++ ": " ++ show freq) relatorioOrdenado
            
            -- 2. SAÍDA: Valor m e Índice de Similaridade Final
            let m = calculaM map1 map2
            let similaridade = calculaSimilaridade map1 map2
            
            putStrLn "\n=== ANÁLISE DE SIMILARIDADE ==="
            putStrLn $ "Valor acumulado m: " ++ show m
            printf "Índice de similaridade: %.2f%%\n" (similaridade * 100)