{-
O Makefile só funciona em Linux ou MAC. Se tiver WSL instalado, só digitar "make" no terminal que vai compilar e executar. Se tiver Git instalado, só digitar "mingw32-make" no terminal que também vai. Senão digita ghc -O2 -o similaridade main.hs para compilar e ./similaridade res.txt sep.txt c1.txt c2.txt para executar. Está explicado no Makefile também.

O que foi feito até o momento:
- Recebe os arquivos como argumento da exeução e os transforma em Strings
- Faz o tratamento dessas Strings para separar as palavras de cada arquivo
- Gera os mapas de frequência dos códigos 1 e 2
- Faz umas impressões para teste
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map

-- Recebe os separadores e o texto sujo, e retorna o texto com espaços no lugar dos separadores, para que a "words" separe as palavras do texto depois
limparTexto :: String -> String -> String
limparTexto sep texto = map verificarCaractere texto -- Esse map aplica uma função para cada caractere do texto
  where
    -- Esta é a nossa "receita" aplicada a cada caractere isolado
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

constroiMapaDeFrequencias :: [String] -> [String] -> Map.Map String Int
constroiMapaDeFrequencias reservadas palavrasCod = foldl adicionarPalavra Map.empty palavrasCod
  where
    -- Esta é a função que o foldl vai aplicar para cada palavra da lista
    adicionarPalavra :: Map.Map String Int -> String -> Map.Map String Int
    adicionarPalavra mapaAtual palavra =
        let 
            -- Se for reservada, o peso (frequência) a somar é 2. Senão, 1.
            peso = if palavra `elem` reservadas then 2 else 1
        in 
            -- Insere a palavra com o peso. Se já existir no mapa de frequências, soma ao valor antigo.
            Map.insertWith (+) palavra peso mapaAtual

-- A função main é o ponto de entrada e tem o tipo IO ()
main :: IO ()
main = do
    -- Captura os argumentos passados na linha de comando da execução
    args <- getArgs
    
    -- Valida se o usuário passou exatamente 4 arquivos
    if length args /= 4
        then do
            putStrLn "Erro: Número incorreto de argumentos."
            putStrLn "Uso correto: ./similaridade <arquivo_res> <arquivo_sep> <arquivo_c1> <arquivo_c2>"
            exitFailure
        else do
            -- Desempacota a lista de argumentos em variáveis
            let [arqRes, arqSep, arqC1, arqC2] = args
            
            -- Lê o conteúdo de todos os arquivos como Strings
            strRes <- readFile arqRes
            strSep <- readFile arqSep
            strC1 <- readFile arqC1
            strC2 <- readFile arqC2
            
            -- Tratamento inicial das palavras
            let palavrasRes = words strRes
            let separadores = words strSep
            let palavrasC1 = tokenize strSep strC1
            let palavrasC2 = tokenize strSep strC2
            
            -- Imprime as listas de palavras que temos para teste
            putStrLn "Arquivos carregados com sucesso!"
            putStrLn $ "Palavras reservadas carregadas: " ++ show (length palavrasRes)
            putStrLn $ "Separadores carregados: " ++ show (length separadores)
            putStrLn $ "Palavras do código 1 carregados: " ++ show (length palavrasC1)
            putStrLn $ unlines palavrasC1
            putStrLn $ "Palavras do código 2 carregados: " ++ show (length palavrasC2)
            putStrLn $ unlines palavrasC2

            -- Gerando os mapas de frequência
            let map1 = constroiMapaDeFrequencias palavrasRes palavrasC1
            let map2 = constroiMapaDeFrequencias palavrasRes palavrasC2
            
            -- Imprime os mapas no terminal para teste
            putStrLn "--- Mapa de Frequências do Código 1 ---"
            print map1
            
            putStrLn "--- Mapa de Frequências do Código 2 ---"
            print map2
            
            return ()
