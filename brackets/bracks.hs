module Main (main) where

import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{-# NOINLINE numHandles #-}
numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0

data Handle = Handle

openFile :: FilePath -> IO Handle
openFile _ = do
  modifyIORef' numHandles succ
  return Handle

hClose :: Handle -> IO ()
hClose _ = modifyIORef numHandles pred

hPutStr :: Handle -> String -> IO ()
hPutStr _ _ = return ()

test :: IO () -> IO ()
test action = do
  action `E.catch` \e ->
    putStrLn $ "exception: " ++ show (e :: SomeException)
  readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket allocate release use = 
  E.mask $ \restore -> do
    resource <- allocate
    restore (use resource)
      `E.finally` release resource

example :: IO ()
example = do
  h <- openFile "path"
  hPutStr h "Hello"
  error "something went wrong"
  hClose h

exampleB = bracket (openFile "path") hClose $ \h -> do
  hPutStr h "Hello"
  error "something went wrong"
  hPutStr h "World"

exampleT = void $ timeout (1*1000*1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2*1000*1000)

main :: IO ()
main = test exampleT

