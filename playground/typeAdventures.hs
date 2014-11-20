{-# LANGUAGE TypeFamilies #-}
module Main where

class Key k where
    data Map k :: * -> *
    empty      :: Map k v
    lookup     :: k -> Map k v -> Maybe v

instance Key Bool where
    data Map Bool v        = MB (Maybe v) (Maybe v)
    empty                  = MB Nothing Nothing
    lookup True (MB _ mt)  = mt
    lookup False (MB mf _) = mf

class Memo k where
    data Table k :: * -> *
    toTable      :: (k -> r) -> Table k r
    fromTable    :: Table k r -> k -> r

memo :: Memo k => (k -> r) -> k -> r
memo = fromTable . toTable

instance Memo Bool where
    data Table Bool w       = TBool w w
    toTable f               = TBool (f True) (f False)
    fromTable (TBool x y) b = if b then x else y

instance Memo a => Memo [a] where
    data Table [a] w = TList w (Table a (Table [a] w))
    toTable f = TList (f [])
                      (toTable (\x ->
                        toTable (\xs -> f (x:xs))))
    fromTable (TList t _) [] = t
    fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs
