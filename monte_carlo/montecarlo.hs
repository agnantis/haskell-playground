module Main where

import Control.Monad (replicateM)
import System.Random

pi' :: Float
pi' = 3.14

-- montecarlo
--
mcArea :: Int -> Float -> IO Float
mcArea it r = do
    let whole = r*r
        p a b = a*a + b*b 
    randomXs <- replicateM it $ randomRIO (0, r) -- IO [Float]
    randomYs <- replicateM it $ randomRIO (0, r) -- IO [Float]
    let size = length . filter (<= whole) . fmap (uncurry p) $ zip randomXs randomYs
    return $ 4 * (fromIntegral size) * whole / (fromIntegral it)


main :: IO ()
main = do
    putStrLn "Circle radius: "
    s <- fmap (read :: String -> Float) getLine
    putStrLn "Repetitions: "
    r <- fmap (read :: String -> Int) getLine
    putStrLn  "-----------"
    putStrLn $ "Circle radius: " ++ show s
    putStrLn $ "Circle area computed: " ++ show (s * s * pi)
    mc <- mcArea r s
    putStrLn $ "Circle area monte carlo: " ++ show mc

