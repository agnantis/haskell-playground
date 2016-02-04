{-# LANGUAGE Rank2Types #-}
module Main where

import Control.Monad (void)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Applicative ((<|>))
import Lens.Family2.Unchecked (Traversal')


-- data definitions
data BValue = String ByteString
            | Number Integer
            | List [BValue]
            | Dictionary [(ByteString, BValue)]
            deriving (Show)

-- parser
string :: Parser BValue
string = P.decimal >>= (\n -> P.char ':' *> (String <$> P.take n))

number :: Parser BValue
number = Number <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')

value :: Parser BValue
value = number <|> string <|> list <|> dict

list :: Parser BValue
list = List <$> (P.char 'l' *> P.many' value)

dict :: Parser BValue
dict = Dictionary <$> (P.char 'd' *> P.many' pair)
  where
    pair = (,) <$> (extract <$> string) <*> value
    extract (String bs) = bs

-- traverse data
bstring :: Traversal' BValue ByteString
bstring f (String bs) = String <$> f bs
bstring _ bv = pure bv


bnumber :: Traversal' BValue Integer
bnumber f (Number i) = Number <$> f i
bnumber _ bv = pure bv

blist :: Traversal' BValue BValue
blist f (List xs) = List <$> traverse f xs
blist _ bv = pure bv

bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case lookup k m of
                               Just v -> f v
                               Nothing -> pure bv
bkey _ _ bv = pure bv
