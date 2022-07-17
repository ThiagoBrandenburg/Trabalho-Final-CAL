import System.IO
import System.Random
import Data.Char
--import Data.Mod.Word
--import Crypto.Random

--____GERACAO DE NUMEROS RANDOMICOS____________________________________________________________________
--mkStdGen n é o gerador padrão de valores, declare um na main e passe por argumento
--Retorna Inteiro entre x e y
-- StdGen = mkStdGen n
-- na main = gen <- getStdGen

--Retorna *primo entre 2 na e 2 na n+1s
primoRange2 :: Integer -> StdGen -> (Integer,Integer)
primoRange2 n gen =
    let 
        k = 2^n
        (a,gen2) = randomR (k,k*2) gen
        (b,gen3) = randomR (k,k*2) gen
        ax = proximo_primo a
        bx = proximo_primo b
    in
        if ax == bx then (ax, proximo_primo (bx+2)) else (ax,bx) 

--retorna um Inteiro entre 0 e 2^n
randBigPow2 :: Integer -> StdGen -> Integer
randBigPow2 k gen = let (x,_) = randomR (0,2^k) gen in x

{-
ModPow a b c = retorna a^b mod c de forma otimizada

modPow :: Integer -> Integer -> Integer -> Integer
modPow a b 1 = 0    --Resto da divisão por 1 sempre é zero
modPow a 0 c = 1    -- a⁰ = 1, 1 mod n sempre será 1, exceto se c=1, mas já tratado acima
modPow a b c = 
    if mod e 2 == 1 then
        modPow 
-}

modPow :: Integer -> Integer -> Integer -> Integer -> Integer
modPow b e 1 r = 0
modPow b 0 m r = r
modPow b e m r
  | e `mod` 2 == 1 = modPow b' e' m (r * b `mod` m)
  | otherwise = modPow b' e' m r
  where
    b' = b * b `mod` m
    e' = e `div` 2

--____TESTES DE PRIMALIDADE______________________________________________________________
--FatoracaoPorDois = Seja n impar na forma n=2⁽s⁾d + 1, retorna (s,d)
fatoracaoPorDois :: Integer -> (Integer, Integer)
fatoracaoPorDois n =
    aux (0,n-1)
    where
        aux :: (Integer, Integer) -> (Integer, Integer)
        aux (s,d) = let nx = mod d 2
            in if nx == 0 then
                aux (s+1,div d 2)
            else (s,d)
        

{-
Teste de Primalidade de Miller Rabin
Sendo n um numero e k o n de iteracoes, retorna
True se n provavelmente é um primo
False se n não é um primo
-}
m_r_primalidade :: Integer -> Int -> Bool
m_r_primalidade _ 0 =
    True
m_r_primalidade n k =
    let 
        (s,d) = fatoracaoPorDois n
        (a,_) = randomR (2,n-2) (mkStdGen k) -- d é um bom valor para seed, visto que d sempre será um numero diferente
        x = modPow a d n 1;
    in
        if x ==1 || x == (n-1) then m_r_primalidade n (k-1) -- Original era x ==1 || x == (n-1) = Continue, 
        else teste x n (s-1) k
        where
            teste :: Integer -> Integer -> Integer -> Int -> Bool
            teste x n 0 k = False
            teste x n s k = 
                let x1 = modPow x 2 n 1 in
                if x1 == n-1 then m_r_primalidade n (k-1)
                else teste x1 n (s-1) k
   
    
ehPrimo :: Integer -> Int -> Bool
ehPrimo 1 _ = False
ehPrimo 2 _ = True
ehPrimo n k = 
    if mod n 2 == 0 then False else m_r_primalidade n k


--Gera um primo aleatorios no intervalo 2^k
primo :: Integer -> StdGen -> Integer
primo k gen = 
    let valor = randBigPow2 k gen in
        if mod valor 2 == 0 then aux (valor+1)
        else aux valor
    where
        aux :: Integer -> Integer
        aux n = if m_r_primalidade n 10 == True then n
        else aux (n+2)

--
proximo_primo :: Integer -> Integer
proximo_primo n = if mod n 2 == 0 then proximo_primo (n+1) else
    if m_r_primalidade n 10 == True then n else aux (n+2) where
        aux :: Integer -> Integer
        aux n = if m_r_primalidade n 10 == True then n
        else aux (n+2)

primo_seguinte :: Integer -> Integer
primo_seguinte n = if mod n 2 == 0 then aux (n+1) else (n+2)
    where
        aux :: Integer -> Integer
        aux n = if m_r_primalidade n 10 == True then n else aux (n+2)

primo_anterior :: Integer -> Integer
primo_anterior n = if mod n 2 == 0 then aux (n-1) else aux (n-2)
    where
        aux :: Integer -> Integer
        aux n = if m_r_primalidade n 10 == True then n else aux (n-2)

--____CRIPTOGRAFIA RSA_______________________________________________________________
{-
Algoritmo de Euclides Estendido
seja mmd(a,b) = ax + by (sendo um dos valores negativos), retorna (a,b)
-}
euclides_ext:: Integer -> Integer -> (Integer, Integer)
euclides_ext x 0 = (1,0)--divisao por 0 sempre da 1 com resto 0
euclides_ext x y = 
    let
        (quociente, resto) = quotRem x y --quotRem Retorna o valor da divisão e o resto em uma tupla
        (novo_quociente, novo_resto) = euclides_ext y resto
    in
        (novo_resto, novo_quociente - quociente*novo_resto)--(old_r, r) := (r, old_r - quotient * r), renomeei old para atual e atual para novo



--Retorna a chave pública e a privada ((n,e),(n,d))
rsa_chave :: (Integer,Integer) -> ((Integer,Integer),(Integer,Integer))
rsa_chave (p,q) =
    let 
        n = p*q
        phi = (p-1)*(q-1)
        e = aux 3 phi   -- e é o primo relativo de phi
        d = if fator < 0 then fator+2*phi else fator
        (fator,_) = euclides_ext e phi
        aux k phi =
            if gcd k phi > 1 then aux (k+2) phi
            else k
    in
        ((n,e),(n,d))

rsa_encripta :: (Integer,Integer) -> Integer -> Integer
rsa_encripta (n,e) x = modPow x e n 1

rsa_decripta :: (Integer,Integer) -> Integer -> Integer
rsa_decripta (n,d) x = modPow x d n 1


--Manipulação dos dados (ord:: Char -> Int) (chr :: Int -> Char)
str_to_list_int :: String -> [Int]
str_to_list_int (x:[]) = [ord x]
str_to_list_int (x:xs) = ord x: str_to_list_int xs

list_int_to_str :: [Int] -> String
list_int_to_str (x:[]) = [chr x]
list_int_to_str (x:xs) = chr x:list_int_to_str xs


string_to_Integer :: String -> Integer
string_to_Integer x = 
    let
        aux :: String -> Integer
        aux [] = 0
        aux (x:xs) = 
            let valor = toInteger (ord x) in valor + (256*(aux xs))
    in
        aux (reverse x)

integer_to_String :: Integer -> String
integer_to_String x =
    let
        aux :: Integer -> String
        aux 0 = []
        aux z = 
            let
                (quociente,resto) = quotRem z 256
                y = chr (fromInteger resto)
            in
                y: aux quociente
    in
        reverse (aux x)

--recebe uma lista, retorna a mesma fragmentada em uma listas de listas de tamanho n
chuncks :: Int -> [a] -> [[a]]
chuncks _ [] = []
chuncks n x = 
    (take n x) : chuncks n (drop n x)
        
{-
Quebra de chave:
O que queremos?
    p e q, tal que p*q=n, e p e q são primos
O que sabemos?
    p,q < sqrt n
    p,q não são pares
-}
lista_primos :: (Integer,Integer) -> [Integer]
lista_primos (x0,x) = 
    if mod x0 2 == 0 then aux (x0+1) (x-x0)
    else aux (x0) (x-x0)
    where
        aux :: Integer -> Integer -> [Integer]
        aux n (-1) = []
        aux n 0 = []
        aux n k = 
            let nx = aux (n+2) (k-2)
            in if m_r_primalidade n 10==True then n:nx else nx

--p é o menor da lista
--q é o maior
--se p*q é > n
--então não existe outro p tal que p*q>n, porque p já é o menor



rsa_forca_bruta :: (Integer,Integer) -> (Integer,Integer)
rsa_forca_bruta (n,e) = rsa_fat n (3,n)

rsa_fat :: Integer -> (Integer,Integer) -> (Integer,Integer)
rsa_fat n (a,b) = 
  if mod a 2 ==0 then rsa_fat n (a+1,b) else
  if mod b 2 ==0 then rsa_fat n (a,b-1) else
  aux n (a,b)
  where
      aux n (a,b) =
        if a > b then (0,0) else
        let nx = a*b in if nx == n then (a,b) else
            if nx > n then aux n (a,b-2) else aux n (a+2,b)





--____MANIPULAÇÂO DOS ARQUIVOS____________________________________________
--main :: IO ()
--main_escreve_chave = Escreve as chaves publica e privada nos seus respectivos arquivos .key

listaStrInteger :: [String] -> [Integer]
listaStrInteger [] = []
listaStrInteger (x:xs) = let y = string_to_Integer x in y:listaStrInteger xs


main_chave :: Integer -> IO ()
main_chave bits = do
    print "Geracao da Chave:"
    leitorChavePublica <- openFile "chavePublica.key" WriteMode
    leitorChavePrivada <- openFile "chavePrivada.key" WriteMode
    gen <- newStdGen
    
    let (p,q) = primoRange2 (quot bits 2) gen
        (c_publica,c_privada) = rsa_chave (p,q)
    print $ "(p,q)=" ++ show (p,q) ++ ",(n,d)=" ++ show c_publica ++ ", (n,e)=" ++ show c_privada
    hPutStrLn leitorChavePublica (show c_publica)
    hPutStrLn leitorChavePrivada (show c_privada)
    hClose leitorChavePublica
    hClose leitorChavePrivada


main_encripta bits = do
    print "Encriptacao do Arquivo"
    leitorPublica <- openFile "chavePublica.key" ReadMode
    leitorArquivo <- openFile "texto_original.txt" ReadMode
    leitorCript <- openFile "texto_encriptado.txt" WriteMode
 
    chave <- hGetLine leitorPublica
    conteudo <- hGetContents leitorArquivo
    let (n,e) = read (chave) :: (Integer,Integer)
        tamanho_ascii = quot bits 8
        valores = map string_to_Integer (chuncks tamanho_ascii conteudo)
        valores_cript = map (rsa_encripta (n,e)) valores
    
    hPutStrLn leitorCript (show valores_cript)

    hClose leitorPublica
    hClose leitorArquivo
    hClose leitorCript

    print $ "Feito, resultado = " ++ show valores_cript


main_decripta = do
    print "Decriptacao do Arquivo:"
    leitorPrivada <- openFile "chavePrivada.key" ReadMode
    leitorEncript <- openFile "texto_encriptado.txt" ReadMode
    leitorDecript <- openFile "texto_decriptado.txt" WriteMode

    chave <- hGetLine leitorPrivada
    conteudo <- hGetContents leitorEncript
    let (n,d) = read (chave) :: (Integer,Integer)
        valores = read conteudo :: [Integer]
        valores_decript = map (rsa_decripta (n,d)) valores
        texto_decript = map integer_to_String valores_decript
        decript = concat texto_decript

    hPutStrLn leitorDecript decript

    hClose leitorPrivada
    hClose leitorEncript
    hClose leitorDecript

    print decript

main_quebra = do
    print "Quebra da Chave:"
    leitorPublica <- openFile "chavePublica.key" ReadMode
    leitorEncript <- openFile "texto_encriptado.txt" ReadMode
    leitorRsa <- openFile "texto_rsa_quebrado.txt" WriteMode

    chave <- hGetLine leitorPublica
    conteudo <- hGetContents leitorEncript
    let (n,e) = read chave :: (Integer,Integer)
        (p,q) = rsa_forca_bruta (n,e)
        ((_,_),(n1,d)) = rsa_chave (p,q)
        valores = read conteudo :: [Integer]
        valores_decript = map (rsa_decripta (n1,d)) valores
        texto_decript = concat $ map integer_to_String valores_decript
    
    print $ "Quebra Realizada, (p,q) = " ++ show (p,q) ++ ", " ++ texto_decript
    hPutStrLn leitorRsa texto_decript

    hClose leitorPublica
    hClose leitorEncript
    hClose leitorRsa

main_limpa = do
    l1 <- openFile "texto_encriptado.txt" WriteMode
    l2 <- openFile "texto_decriptado.txt" WriteMode
    l3 <- openFile "texto_rsa_quebrado.txt" WriteMode
    
    hPutStrLn l1 ""
    hPutStrLn l2 ""
    hPutStrLn l3 ""

    hClose l1
    hClose l2
    hClose l3

main = do
    print "Trabalho CAL: Algoritmo RSA e quebra por forca bruta"
    print "Digite a ordem de grandeza da chave rsa (2^input) ="
    n <- getLine
    print "Digite a ordem de grandeza dos chuncks (2^(8*input)"
    k <- getLine
    let nx = read n :: Integer
        kx = read k :: Int
    main_chave nx
    main_encripta kx
    main_decripta
    main_quebra
    print "Execucao Finalizada"
