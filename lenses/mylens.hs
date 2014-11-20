module MyLens where

type Lens s a = (a -> a) -> s -> (a, s)

ix :: Int -> Lens [Int] Int
ix index f list
  | index < 0     = error "index too small"
  | null list     = error "index too large"
  | x:xs <- list  = if (index == 0)
                    then (x, (f x):xs)
                    else second (x:) $ ix (index-1) f xs
                                
second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)

