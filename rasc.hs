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


rsa_forca_bruta :: (Integer,Integer) -> (Integer,Integer)
rsa_forca_bruta (n,e) =
    let
        lista = lista_primos 3 (sqrt n)
        (p,q) = aux n lista
        where
            aux :: Integer -> [Integer] -> (Integer,Integer)

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

   main_gera_chave = do
    chavePublica <- openFile "chavePublica.key"
    chavePrivada <- openFile "chavePrivada.key"
    gen <- newStdGen
    let ((n,e),(n,d)) = rsa_chave 


block_string_l_integer :: Int -> String -> [Integer]
block_string_l_integer k [] = []
block_string_l_integer k block =
    let
        (a,b) = aux k block
        --aux recebe uma lista, e separa em duas listas passadas em uma tupla, a primeira de tamanho k e a segunda o resto
        aux k block :: [Integer] -> ([Integer],[Integer])
    concat a block_string_l_integer b



main_encripta_arquivo = do
    --Leitura da Chave
    leitorPublica <- openFile "chavePublica.key" ReadMode
    leitorArquivo <- openFile "texto_original.txt" ReadMode
    leitorCript <- openFile "texto_encriptado.txt" WriteMode
 
    chave <- hGetLine leitorPublica
    conteudo <- hGetContents leitorArquivo
    let (n,e) = read (chave) :: (Integer,Integer)
        valores = map string_to_Integer (chuncks 2 conteudo)
        valores_cript = map (rsa_encripta (n,e)) valores
        --texto_cript = unlines $ map integer_to_String valores_cript
        texto_cript = map integer_to_String valores_cript
        cript = unlines texto_cript
    print "Conteudo"
    print conteudo
    print "Valores Em Inteiros"
    print valores
    print "Valores Encriptados"
    print valores_cript
    print "Texto Encriptado"
    print texto_cript

    hPutStrLn leitorCript cript

    print "pronto\n"
    hClose leitorPublica
    hClose leitorArquivo
    hClose leitorCript

main_decripta_arquivo = do
    leitorPrivada <- openFile "chavePrivada.key" ReadMode
    leitorEncript <- openFile "texto_encriptado.txt" ReadMode
    leitorDecript <- openFile "texto_decriptado.txt" WriteMode

    chave <- hGetLine leitorPrivada
    conteudo <- hGetContents leitorEncript
    let (n,d) = read (chave) :: (Integer,Integer)
        linhas = lines conteudo
        valores = map string_to_Integer (map concat $ chuncks 2 linhas)
        valores_decript = map (rsa_decripta (n,d)) valores
        texto_decript = map integer_to_String valores_decript
        decript = concat texto_decript
    print "Texto Encriptado"
    print linhas
    print "Valores Encriptados\n"
    print valores
    print "Valores Decriptados\n"
    print valores_decript
    print "Texto Caraio"
    print texto_decript

    hPutStrLn leitorDecript decript

    hClose leitorPrivada
    hClose leitorEncript
    hClose leitorDecript

main = do
    gen <- newStdGen
    print (primo 100 gen)

not_main = do
    gen <- newStdGen
    --print (primo 100 gen)
    print (primoRange2 10 gen)

not_main2 = do
    leitor <- openFile "texto_original.txt" ReadMode
    conteudo <- hGetContents leitor
    print (lines conteudo)
