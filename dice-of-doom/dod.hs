{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main, createBoard) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.List (findIndices)
import Data.List.Split (chunksOf)
import System.Random

type Player   = Char
type Dice     = Int
type Index    = Int
type Position = (Player, Dice)
type Board    = [Position]
type Move     = (Index, [Index])

data Game = Game {
              size :: Int,
              players:: [Player],
              board :: Board,
              current :: Int
            }

-----------------------------------------
-- instances
-----------------------------------------

instance Random Position where
    randomR ((ap, ad), (bp, bd)) g = ((pl, dl), g'')
      where (pl, g')  = randomR (ap, bp) g
            (dl, g'') = randomR (ad, bd) g'
    random = randomR ((maxBound, maxBound), (maxBound, maxBound))

reprPosition :: Position -> String
reprPosition (p,d) = p : '-' : show d

buildGame :: Int -> Int -> Int -> IO Game
buildGame s p m = do
    (plrs, brd) <- createBoard s p m
    return Game {
                size    = s,
                players = plrs,
                board   = brd,
                current = 0
           }

createBoard :: Int -> Int -> Int -> IO ([Player], Board)
createBoard boardSize playerNo maxDices = do
    let sz   = boardSize*boardSize
        minB = ('a', 1)
        maxP = ['a'..] !! (playerNo - 1)
        maxB = (maxP, maxDices)
    g <- getStdGen
    return (take playerNo ['a'..], take sz $ randomRs (minB, maxB) g)

availableMoves :: Game -> [Move]
availableMoves g = filter (not.null.snd) $ zip pos (fmap (availableMove g) pos)
  where pl  = players g !! current g
        brd = board g
        pos = findIndices ((==pl).fst) brd 
        

availableMove :: Game -> Index -> [Index]
availableMove g inx = lowerP
  where brd       = board g
        (plr, dc) = brd !! inx
        neigh     = neighbor g inx
        lowerP    = filter fltr neigh  
        fltr x    = plr' /= plr && dc' < dc
          where (plr', dc') = brd !! x  

neighbor :: Game -> Index -> [Index]
neighbor g inx = filter ((&&) <$> (>=0) <*> (<mx*mx)) neigh
  where mx    = size g
        col   = mod inx mx
        neigh = [inx-mx, inx+mx] ++
          (if col == 0 then [] else [inx-1, inx+mx-1]) ++    -- left edge
          (if col == (mx-1) then [] else [inx+1, inx-mx+1]) -- right edge

moveDices :: Game -> Index -> Index -> Game
moveDices g f t = g { board = toB }
  where (fp, fd) = board g !! f
        (tp, td) = board g !! t
        frB      = updateList (board g) f (fp, 1)
        toB      = updateList frB t (fp, fd-1)

updateList :: [a] -> Int -> a -> [a]
updateList xs inx nv = f ++ (nv:tail b)
  where (f, b) = splitAt inx xs
--
printBoard :: Int -> Board -> String
printBoard sz b = unlines $ fmap liner stringRep
    where stringRep       = zip [0..] $ chunksOf sz $ fmap reprPosition b
          liner (i, line) = replicate (2*i) ' ' ++ unwords line

printAvailMove :: [Move] -> IO (Index, Index)
printAvailMove mvs = do
    putStrLn "Available move(s):"
    forM_ zipped (\(key, (from, to)) -> putStrLn $ "\t" ++ show (key :: Int) ++ ". " ++ show (from+1) ++ " -> " ++ show (to+1))
    putStr $ "Choose your move [1-" ++ show sz ++ "]: "
    sel <- read <$> getLine
    if sel > 0 && sel <= sz then
      return $ snd $ zipped !! (sel - 1)
    else do
        putStrLn "Invalid Retry..."
        printAvailMove mvs
  where pairs mv = fmap ((,) (fst mv)) (snd mv)
        flatt    = concatMap pairs mvs
        zipped   = zip [1..] flatt
        sz       = length zipped


--------------------------------------
-- main loop
--------------------------------------
nextStep :: Game -> IO ()
nextStep game = do
    let moves = availableMoves game  
        brd   = board game
        sz    = size game
        pl    = players game !! current game
    putStrLn $ printBoard sz brd
    putStrLn $ "Player " ++ show pl
    (f, t) <- printAvailMove moves
    putStrLn $ "Moving dices from " ++ show f ++ " to " ++ show t
    let mGame = moveDices game f t
    nextStep $ mGame { current = (current mGame + 1) `mod` (length . players $ mGame) }


main :: IO ()
main = do
    let players  = 2
        bsize    = 3
        maxDices = 4
        title    = "--- Dice of Doom ---"
        border   = replicate (length title) '-'
    putStrLn border 
    putStrLn title
    putStrLn border
    game <- buildGame bsize players maxDices
    nextStep game
