module Dom where

import qualified Data.Text as T

data NTree a = NTree a [NTree a]
  deriving (Show)

data NodeType = Text T.Text
              | Element ElementData

main :: IO ()
main = (putStrLn . show) $ 3
