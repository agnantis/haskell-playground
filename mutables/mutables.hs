module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ST
import Data.IORef
import Data.STRef


-- IORef buble sort example --
bubbleSortIO :: [Int] -> IO [Int]
bubbleSortIO b = do
    let ln = length b
    counter <- newIORef 0
    xs <- mapM newIORef b
    forM_ [0 .. (ln-1)] $ \i -> do
      forM_ [0 .. (ln-i-2)] $ \j -> do
        modifyIORef counter (+1)
        let ix = xs !! j
        let iy = xs !! (j+1)
        x <- readIORef $ ix
        y <- readIORef $ iy
        when (x > y) $ do
          writeIORef ix y
          writeIORef iy x
    its <- readIORef counter
    putStrLn $ "Iterations: " ++ (show its) 
    mapM readIORef xs

-- IORef buble sort example --

-- STRef buble sort example --
bubbleSortST :: [Int] -> [Int]
bubbleSortST b = runST $ do
    let ln = length b
    xs <- mapM newSTRef b
    forM_ [0 .. (ln-1)] $ \i -> do
      forM_ [0 .. (ln-i-2)] $ \j -> do
        let ix = xs !! j
        let iy = xs !! (j+1)
        x <- readSTRef $ ix
        y <- readSTRef $ iy
        when (x > y) $ do
          writeSTRef ix y
          writeSTRef iy x
    mapM readSTRef xs

-- STRef buble sort example --

main :: IO ()
main = do
    a <- newEmptyMVar
    forkIO $ forever $ takeMVar a >>= putStrLn

    forever $ do
      text <- getLine
      putMVar a text
