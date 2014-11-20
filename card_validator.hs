module Main where

computeDoubleSum :: String -> String
computeDoubleSum xs = snd $ foldr folder (True, "") xs
  where
    folder ch (flag, acc) = (not flag, if flag then acc else ch:acc) 

main :: IO ()
main = (print . computeDoubleSum) "kostas"
