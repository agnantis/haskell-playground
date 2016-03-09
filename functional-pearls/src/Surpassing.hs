module Surpassing
    ( mscount
    , scount
    ) where

import Data.List (tails)

-- msc: maximum surpassing count
-- example : G E N E R A T I N G
--      sc : 5 6 3 5 1 4 0 1 0 0 (msc: 6)
-- naive implementation
mscount :: Ord a => [a] -> Int
mscount [] = 0
mscount xs = maximum [ scount x zs | (x:zs) <- tails xs ]

scount :: Ord a => a -> [a] -> Int
scount c = length . filter (c<)

-- divide-n-conquer strategy
mscDnC :: Ord a => [a] -> Int
mscDnC = maximum . map snd . table

table :: Ord a => [a] -> [(a, Int)]
table xs = [(x, scount x zs) | (x:zs) <- tails xs ]

