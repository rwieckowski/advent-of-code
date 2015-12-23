{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16
import Data.ByteString.Char8 (pack, unpack)
import Data.List
import Control.Parallel.Strategies

md5 :: String -> String
md5 = unpack . encode . MD5.hash . pack

day4 n p = find (isPrefixOf (replicate n '0') . snd) $ map (\n -> (n, md5 n)) $ map ((p++) . show) [1..]

first = day4 5

second = day4 6

main = do
    print (first "bgvyzdsv")
    print (second "bgvyzdsv")