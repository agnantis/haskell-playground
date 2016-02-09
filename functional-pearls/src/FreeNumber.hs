module FreeNumber
    ( someFunc
    , minfreeNaive
    , minfreeArray
    , minfreeDnC
    , minfreeSTArray
    ) where

import Control.Monad (sequence)
import Data.List ((\\), partition)
import Data.Array (accumArray, elems, Array)
import Data.Array.ST (runSTArray)
import Data.Array.MArray (newArray, writeArray)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- Pearl 1: The smallest free number -}
-- naive approach
minfreeNaive :: [Int] -> Int
minfreeNaive [] = 0
minfreeNaive ys = head ([0..] \\ ys)

-- array approach
minfreeArray :: [Int] -> Int
minfreeArray [] = 0
minfreeArray xs = search . checklist $ xs

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) $ zip xs' (repeat True)
  where
    n = length xs
    xs' = filter (< n) xs

-- divide and conquer approach
minfreeDnC :: [Int] -> Int
minfreeDnC xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs) | n == 0   = a
                  | m == b-a  = minfrom b (n-m, vs)
                  | otherwise = minfrom a (m, us)
  where
    (us, vs) = partition (<b) xs
    m = length us
    b = a + 1 + n `div` 2

-- STArray approach
minfreeSTArray :: [Int] -> Int
minfreeSTArray [] = 0
minfreeSTArray xs = search . checklistST $ xs

checklistST :: [Int] -> Array Int Bool
checklistST xs = runSTArray $ do 
    a <- newArray (0,n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a
  where
    n = length xs
