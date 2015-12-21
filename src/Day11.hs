module Day11 (p1, p2) where

import Control.Arrow
import Control.Monad

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

input = "vzbxkghb"

test1 = "abcdefgh"
test2 = "ghijklmn"
test2' = "ghizzzzz"

naughyChars = S.fromList ['i', 'o', 'l']
hasNaughty s = or $ map (`S.member` naughyChars) s

inc :: String -> String
inc = reverse . incLst . reverse
  where incLst [] = []
        incLst (c : cs) = case incLetter c of
          'a' -> 'a' : incLst cs
          d | d `S.member` naughyChars -> incLst (d : cs)
          d -> d : cs
        incLetter 'z' = 'a'
        incLetter c = chr . succ $ ord c

hasStraight (a:rs@(b:c:_))
  | ((succ . succ $ ord a) == ord c) && ((succ $ ord b) == ord c) = True
  | otherwise = hasStraight rs
hasStraight _ = False

hasPair (a:rs@(b: rss))
  | a == b = True
  | otherwise = hasPair rs
hasPair _ = False

has2pair (a:rs@(b: rss))
  | a == b = hasPair rss
  | otherwise = has2pair rs
has2pair _ = False

isNice :: String -> Bool
isNice s = and $ map ($ s) [hasStraight,
                             not . hasNaughty,
                            has2pair]

p1 = head . filter isNice . tail $ iterate inc input
p2 = (!! 1) . filter isNice . tail $ iterate inc input
