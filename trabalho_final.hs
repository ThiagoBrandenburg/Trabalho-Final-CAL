import System.Random

myPureFunction :: Integer -> Integer
myPureFunction x = x*x

rand_provisorio :: Int
rand_provisorio = fst (next (mkStdGen 1))

drawInt :: Integer -> Integer -> IO Integer
drawInt x y = getStdRandom (randomR (x,y))




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