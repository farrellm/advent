module Day4 ( ) where

import Control.Arrow
import Data.Hash.MD5

input = "ckczppom"

hash :: Int -> String
hash = md5s . Str . (input `mappend`) . show

hs = map (id &&& hash) [1..]

isCoin ('0' : '0' : '0' : '0' : '0' : _) = True
isCoin _ = False

p1 = head $ filter (isCoin . snd) hs

isCoin2 ('0' : '0' : '0' : '0' : '0' : '0' : _) = True
isCoin2 _ = False

p2 = head $ filter (isCoin2 . snd) hs
