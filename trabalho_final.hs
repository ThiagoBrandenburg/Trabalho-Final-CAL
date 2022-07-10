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
randBigRange :: Integer -> Integer -> StdGen-> Integer
randBigRange x y gen = let (z,w) = randomR (x,y) gen
    in z

--retorna um Inteiro entre 0 e 2^n
randBigPow2 :: Integer -> StdGen -> Integer
randBigPow2 k gen = randBigRange 0 (2^k) gen

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
        a = randBigRange 2 (n-2) (mkStdGen k) -- d é um bom valor para seed, visto que d sempre será um numero diferente
        x = modPow a d n 1;
    in
        if x ==1 || x == (n-1) then m_r_primalidade n (k-1) -- Original era x ==1 || x == (n-1) = Continue, 
        else teste x n (s-1) k
        where
            teste :: Integer -> Integer -> Integer -> Int -> Bool
            teste x n 0 k = False
            teste x n s k = 
                if modPow x 2 n 1 == n-1 then m_r_primalidade n (k-1)
                else teste x n (s-1) k
   
    
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



--Retorna a chave pública e a privada ((n,e),(p,q,d))
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

--
--Manipulação dos dados (ord:: Char -> Int) (chr :: Int -> Char)
str_to_list_int :: String -> [Int]
str_to_list_int (x:[]) = [ord x]
str_to_list_int (x:xs) = ord x: str_to_list_int xs

list_int_to_str :: [Int] -> String
list_int_to_str (x:[]) = [chr x]
list_int_to_str (x:xs) = chr x:list_int_to_str xs

integer_to_String :: Integer -> String
integer_to_String x = show x

string_to_Integer :: String -> Integer
string_to_Integer x = read x 



--main :: IO ()
main = do
    gen <- newStdGen
    print (primo 20 gen)
    