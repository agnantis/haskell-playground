module Main (solve, solve') where

xs :: [(String, Either String Int)]
xs = [ ("a", Left "b"),
       ("b", Right 42),
       ("c", Left "a"),
       ("d", Left "e")]


solve :: [(String, Either String Int)] -> [(String, Maybe Int)]
solve mp = fmap (solve' mp) mp

solve' :: [(String, Either String Int)] -> (String, Either String Int) -> (String, Maybe Int)
solve' mp (k, Right val) = (k, Just val)
solve' mp (k, Left ref) = case lookup ref mp of
                            Nothing -> (k, Nothing)
                            Just sm -> solve' mp (k, sm)
