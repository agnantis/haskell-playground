module Main(main) where 
import Control.Monad (guard)
import Data.List

{-
-     S E N D
-   + M O R E
-   ---------
-   M O N E Y
-}

digits :: [Int]
digits = [0..9]

--filterOut :: [Int] -> [Int] -> [Int]
--filterOut [] xs = xs
--filterOut = 

toNumber :: [Int] -> Int
toNumber = foldl' (\a b -> a*10 + b) 0

decode = do
  s <- digits \\ [0]
  e <- digits \\ [s]
  n <- digits \\ [s,e]
  d <- digits \\ [s,e,n]
  m <- digits \\ [0,s,e,n,d]
  o <- digits \\ [s,e,n,d,m]
  r <- digits \\ [s,e,n,d,m,o]
  y <- digits \\ [s,e,n,d,m,o,r]
  let send  = toNumber[s,e,n,d]
      more  = toNumber[m,o,r,e]
      money = toNumber[m,o,n,e,y]
  guard $ send + more == money
  return (send, more, money)

main :: IO ()
main = print decode


