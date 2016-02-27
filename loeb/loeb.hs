import Control.Monad
import Data.Array
import Data.List
import Text.Printf

val :: a -> b -> a
val = const

e :: b -> Double
e = val 0.0

loeb :: Functor f => f (f a -> a) -> f a
loeb xs = go where go = fmap ($ go) xs
--loeb xs = fmap ($ loeb xs) xs

vat :: (Num a, Ix i) => a -> i -> Array i a -> a 
vat v i = (* v) . (! i)

sum' :: (Num a, Ix i) => [i] -> Array i a -> a
sum' xs ar = sum . fmap (ar !) $ xs

spreadsheet :: Array (Integer, Integer) (Array (Integer, Integer) Double -> Double)
spreadsheet = listArray ((0,0), (3,3))
    [ val 10,                      vat 0.1 (0,0), sum' [(0, i) | i <- [0..1]], e
    , val 15,                      vat 0.1 (1,0), sum' [(1, i) | i <- [0..1]], e
    , val 22,                      vat 0.2 (2,0), sum' [(2, i) | i <- [0..1]], e
    , sum' [(i, 0) | i <- [0..2]], e,             sum' [(i, 2) | i <- [0..2]], e
    ]

printArr :: Array (Integer, Integer) Double -> IO ()
printArr ar =
  forM_ [0..3] $ \r -> do
    forM_ [0..3] $ \c ->
      printf "%4.2f   " (ar ! (r, c))
    printf "\n"

main:: IO ()
main = printArr $ loeb spreadsheet
