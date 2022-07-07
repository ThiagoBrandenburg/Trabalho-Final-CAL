import System.Random
import Crypto.Random
--import Data.mod
--import Data.Mod.Word

{-
Geração de Numeros Randomicos
-}
myPureFunction :: Integer -> Integer
myPureFunction x = x*x

rand_provisorio :: Int
rand_provisorio = fst (next (mkStdGen 1))

--Esse é o que utilizamos
{-
randBigIntRange :: Integer -> Integer -> IO Integer
randBigIntRange x y = getStdRandom (randomR (x,y))
-}


--Retorna Inteiro entre x e y
randBigRange :: Integer -> Integer -> Int -> Integer
randBigRange x y n = let (z,w) = randomR (x,y) (mkStdGen n)
    in z



--retorna um Inteiro entre 0 e 2^n
randBigPow2 :: Integer -> Integer -> Integer
randBigPow2 k seed= randBigRange 0 (2^k) (fromIntegral seed)

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
        

--

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
        a = randBigRange 2 (n-2) k -- d é um bom valor para seed, visto que d sempre será um numero diferente
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
primo :: Integer -> Integer -> Integer
primo k seed = 
    let valor = randBigPow2 k seed in
        if mod valor 2 == 0 then aux (valor+1)
        else aux valor
    where
        aux :: Integer -> Integer
        aux n = if m_r_primalidade n 10 == True then n
        else aux (n+2)

--

euclides_ext:: Integer -> Integer -> (Integer, Integer)
euclides_ext x 0 = (1,0)--divisao por 0 sempre da 1 com resto 0
euclides_ext x y = 
    let
        (quociente, resto) = quotRem x y --quotRem Retorna o valor da divisão e o resto em uma tupla
        (novo_quociente, novo_resto) = euclides_ext y resto
    in
        (novo_resto, novo_quociente - quociente*novo_resto)--(old_r, r) := (r, old_r - quotient * r), renomeei old para atual e atual para novo


main :: IO ()
main = do
    -- num :: Float
    num <- randomIO :: IO Integer
    -- This "extracts" the float from IO Float and binds it to the name num
    print $ myPureFunction num 


{-
main2 = do
    g <- newStdGen
    print . take 10 
-}