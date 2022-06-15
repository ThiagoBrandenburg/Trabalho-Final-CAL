import System.Random

myPureFunction :: Integer -> Integer
myPureFunction x = x*x

rand_provisorio :: Int
rand_provisorio = fst (next (mkStdGen 1))

randBigIntRange :: Integer -> Integer -> IO Integer
randBigIntRange x y = getStdRandom (randomR (x,y))

fatoracao :: Integer -> Integer
fatoraca
        

m_r_primalidade :: Integer -> Int -> Bool
m_r_primalidade 1 k =
    False
m_r_primalidade 2 k =
    True
m_r_primalidade n 0 =
    True
mr_primalidade n k =
    




{-
mr_primalidade :: Integer -> Int -> Bool
mr_primalidade n 0 = True
mr_primalidade 
-}




main :: IO ()
main = do
    -- num :: Float
    num <- randomIO :: IO Integer
    -- This "extracts" the float from IO Float and binds it to the name num
    print $ myPureFunction num 