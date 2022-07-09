myPureFunction :: Integer -> Integer
myPureFunction x = x*x

rand_provisorio :: Int
rand_provisorio = fst (next (mkStdGen 1))

rsa_chave :: Integer -> Integer -> 
rsa_chave p q =
    let
        n = p*q
        fi = (p-1)*(q-1) --Funcao totiente de Euler= Numero de coprimos de um numero n

--Retorna chave privada de decriptar (d,n)
rsa_chave_decript :: (Integer,Integer) -> (Integer, Integer)
rsa_chave_decript (,)

rsa_chave_encript

--Retorna a chave pública e a privada de decriptografia (d,n)
rsa_chave_decript :: (Integer,Integer) -> (Integer, Integer)
rsa_chave_decript (p,q) = 
    let
        n = p*q
        phi = (p-1)*(q-1)