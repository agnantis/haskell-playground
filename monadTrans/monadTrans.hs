module Main (main) where 

import Control.Applicative

-- MaybeIO monad --
data MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
  fmap f = MaybeIO . ffmap . runMaybeIO
    where ffmap = (fmap . fmap) f

instance Applicative MaybeIO where
  pure = MaybeIO . pure . pure
  (<*>) (MaybeIO f) (MaybeIO v) = MaybeIO $ liftA2 (<*>) f v

instance Monad MaybeIO where
    return a = pure a
    -- (>>=) :: m a -> (a -> m b) -> m b
    MaybeIO m >>= f = MaybeIO $ m >>= \x -> case x of
      Nothing   -> return Nothing
      Just val  -> runMaybeIO $ f val
-- MaybeIO monad --

-- MaybeT monad --
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . ffmap . runMaybeT
    where ffmap = (fmap . fmap) f

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) (MaybeT f) (MaybeT v) = MaybeT $ liftA2 (<*>) f v

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . return
    MaybeT m >>= f = MaybeT $ do
      x <- m
      case x of
        Nothing   -> return Nothing
        Just val  -> runMaybeT $ f val
-- MaybeT monad --
data User = User deriving Show

findById :: Int -> IO (Maybe User)
findById 1 = return $ Just User
findById _ = return Nothing

findUsers :: Int -> Int -> IO (Maybe (User, User))
findUsers x y = do
    muser1  <- findById x
    
    case muser1 of
      Nothing -> return Nothing
      Just user1 -> do
        muser2 <- findById y

        case muser2 of
          Nothing -> return Nothing
          Just user2 -> return $ Just (user1, user2)

smartFindUsers :: Int -> Int -> IO (Maybe (User, User))
smartFindUsers x y = runMaybeIO $ do
  user1 <- MaybeIO $ findById x
  user2 <- MaybeIO $ findById y

  return (user1, user2)

transformerFindUsers :: Int -> Int -> IO (Maybe (User, User))
transformerFindUsers x y = runMaybeT $ do
  user1 <- MaybeT $ findById x
  user2 <- MaybeT $ findById y

  return (user1, user2)

main :: IO ()
main = do
    putStr "first user id: "
    x <- fmap read getLine
    putStr "second user id: "
    y <- fmap read getLine
    result <- findUsers x y
    putStrLn $ "DB Query result: " ++ show result

