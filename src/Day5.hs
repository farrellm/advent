{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day5 ( ) where

import BasicPrelude
import qualified Data.Set as Set
import qualified Data.Text as T
-- import Control.Arrow

input = readFile "input/day5.txt"

vowels = Set.fromList ['a', 'e', 'i', 'o', 'u']
isVowel = flip Set.member $ vowels

hasPair (a : bs@(b : _))
  | a == b = True
  | otherwise = hasPair bs
hasPair _ = False

naughty = Set.fromList ["ab", "cd", "pq", "xy"]

hasNaughty (a : bs@(b : _))
  | Set.member [a, b] naughty = True
  | otherwise = hasNaughty bs
hasNaughty _ = False

isNice w = length (filter isVowel w) >= 3 &&
           hasPair w &&
           (not . hasNaughty) w

p1 = length . filter (isNice . T.unpack) . lines <$> input

every :: [a -> Bool] -> a -> Bool
every ps = and . (ps <*> ) . pure


rule1 (a : bs@(b : rs))
  | [a, b] `isInfixOf` rs = True
  | otherwise = rule1 bs
rule1 _ = False

rule2 (a : bs@(_ : b : _))
  | a == b = True
  | otherwise = rule2 bs
rule2 _ = False

p2 = length . filter (every [rule1, rule2] . T.unpack) . lines <$> input
