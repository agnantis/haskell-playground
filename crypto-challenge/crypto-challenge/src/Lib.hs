module Lib where

import Data.Char (ord, chr, intToDigit)
import Data.Map.Strict ((!))
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Numeric (showIntAtBase, showHex, readInt)


type HexString = String

messageInHex :: HexString
messageInHex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

-- Decodes a hex encoded string
hex2string :: HexString -> String 
hex2string (x:y:xs) = ch:hex2string xs
  where
    ch = (chr . read) $ "0x" ++ [x,y]
hex2string _ = ""

-- Encodes a string to a hex string
str2Hex :: String -> HexString 
str2Hex = concatMap (\c -> zeroPadding 2 (showHex (ord c) "" ))

-- Encodes a string to its base64 representation
str2base64 :: String -> String
str2base64 xs =
  let padding  = (3 - length xs `mod` 3) `mod` 3
      paddedxs = xs ++ replicate padding (chr 0)
      str2bit  = concatMap ((zeroPadding 8) . int2bin . ord) paddedxs
      bit6seg  = splitEvery 6 str2bit
      str64    = map ((base64Table !) . bin2int) bit6seg
  in take (length str64 - padding) str64 ++ replicate padding '='


-- Decodes a valid base64 string 
base642str :: String -> Maybe String
base642str xs = do
    let padding   = length . takeWhile (== '=') . reverse $ xs
    chr2no  <-  sequence $ map (\x -> Map.lookup x base64TableRev) xs
    let no2bin    = concatMap ((zeroPadding 6) .int2bin) chr2no
        binNoPad  = if padding /= 0 then take (length no2bin - 8 * padding) no2bin else no2bin
        byteSegs  = splitEvery 8 binNoPad
        intSegs   = map bin2int byteSegs
        chrSegs   = map chr intSegs
    return chrSegs

zeroPadding :: Int -> String -> String
zeroPadding n xs | length xs < n  = replicate (n - length xs) '0' ++ xs
                 | otherwise      = xs

int2bin :: Int -> String
int2bin x = showIntAtBase 2 intToDigit x ""

bin2int :: String -> Int
bin2int bin = case readInt 2 (\c -> c == '0' || c == '1') (\c -> read [c]) bin of
  [] -> 0
  [(val, _)] -> val

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs | null xs        = []
                | length xs < n  = [xs]
                | otherwise      = let
                                     (s, xs') = splitAt n xs
                                   in 
                                     s:splitEvery n xs'


base64Table :: Map.Map Int Char
base64Table = Map.fromList $ zip [0..] allBase64Symbols

base64TableRev :: Map.Map Char Int 
base64TableRev = Map.fromList $ ('=', 0) : zip allBase64Symbols [0..]

allBase64Symbols :: String
allBase64Symbols = ['A'..'Z'] ++ ['a'..'z'] ++ map (head . show) [0..9] ++ ['+', '/']

