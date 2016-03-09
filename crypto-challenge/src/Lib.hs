module Lib where

import Control.Monad   (zipWithM)
import Data.Char       (ord, chr, intToDigit, isAsciiLower, isAsciiUpper, isNumber)
import Data.List       (maximumBy)
import Data.Map.Strict ((!))
import Data.Maybe      (fromJust, listToMaybe)
import Data.Ord        (comparing)
import Data.Word       (Word8)
import Numeric         (showIntAtBase, showHex, readHex, readInt)
import qualified Data.Map.Strict       as Map
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C


type HexString    = String
type BinString    = String
type Base64Char   = Char
type Base64String = [Base64Char]
type Byte         = Word8

-------------------
-- helpers
-------------------
optionalHead :: [a] -> Maybe a
optionalHead = listToMaybe

occurences :: Ord a => a -> [a] -> Int
occurences k = length . filter (== k)

zeroPadding :: Int -> String -> String
zeroPadding n xs | length xs < n  = replicate (n - length xs) '0' ++ xs
                 | otherwise      = xs

hex2bin :: HexString -> BinString
hex2bin = concatMap (zeroPadding 8 . int2bin . fst . head . readHex . (:[]))

bin2Hex :: BinString -> HexString
bin2Hex bx = concatMap (flip showHex "" . bin2int) $ splitEvery 8 bx

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

base64Table :: Map.Map Int Base64Char
base64Table = Map.fromList $ zip [0..] allBase64Symbols

base64TableRev :: Map.Map Base64Char Int 
base64TableRev = Map.fromList $ ('=', 0) : zip allBase64Symbols [0..]

allBase64Symbols :: Base64String
allBase64Symbols = ['A'..'Z'] ++ ['a'..'z'] ++ map (head . show) [0..9] ++ ['+', '/']


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
str2base64 :: String -> Base64String
str2base64 xs =
  let padding  = (3 - length xs `mod` 3) `mod` 3
      paddedxs = xs ++ replicate padding (chr 0)
      str2bit  = concatMap (zeroPadding 8 . int2bin . ord) paddedxs
      bit6seg  = splitEvery 6 str2bit
      str64    = map ((base64Table !) . bin2int) bit6seg
  in take (length str64 - padding) str64 ++ replicate padding '='


-- Decodes a valid base64 string 
base642str :: Base64String -> Maybe String
base642str xs = do
    let padding   = length . takeWhile (== '=') . reverse $ xs
    chr2no  <-  mapM (`Map.lookup` base64TableRev) xs
    let no2bin    = concatMap (zeroPadding 6 . int2bin) chr2no
        binNoPad  = if padding /= 0 then take (length no2bin - 8 * padding) no2bin else no2bin
        byteSegs  = splitEvery 8 binNoPad
        intSegs   = map bin2int byteSegs
        chrSegs   = map chr intSegs
    return chrSegs

-- Converts a hex-encoded string to base64-encoded string
hex2base64 :: HexString -> Base64String
hex2base64 = str2base64 . hex2string


-- second challenge - Fixed xor

-- Merges two hex-encoded strings using XOR on their binary representation 
fixedXOR :: HexString -> HexString -> Maybe HexString
fixedXOR xs ys | length xs /= length ys = Nothing
               | otherwise = go
  where
    go = Just . bin2Hex $ zipWith xor (hex2bin xs) (hex2bin ys)
    xor a b = if a == b then '0' else '1'

-- third challenge - single byte XOR
--
singleByteXOR :: HexString -> Byte -> HexString
singleByteXOR xs k = concat $ fromJust decodedHex
  where
    hexk       = zeroPadding 2 $ showHex k ""
    decodedHex = zipWithM (\a b -> fixedXOR [a] [b]) (concat $ repeat hexk) xs

-- Given a string it returns the number of alpharithmetic char
-- It can be used, as a simple heuristic to compare decoded strings
englishHeuristic :: String -> Int
englishHeuristic = length . filter hieristic
  where
    isSpace'    = (== ' ')
    hieristic c = any ($ c) [isAsciiLower, isAsciiUpper, isNumber, isSpace']

-- Decrypts a hex string which has been XOR'd agains a single char
-- Return the key and the decoded string
singleByteXORcipher :: HexString -> (Char, String)
singleByteXORcipher hs = selected
  where
    candidates = map (hex2string . singleByteXOR hs) [0..255]
    pairs      = zip (map chr [0..255]) candidates
    selected   = maximumBy (comparing (englishHeuristic . snd)) pairs

data MyData = SData String | IData Int

--myfunc :: MyData -> Int
--myfunc x = _

f (Just a) = Left a
f Nothing = Right ()
