{-# LANGUAGE CPP #-}
module Main (main) where

go n = do
    if (n `rem` THRESHOLD == 0)
      then putChar '.'
      else return ()
    n' <- go (n+1)
    return (n+n')

main = print =<< go 0
