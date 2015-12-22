{-# LANGUAGE OverloadedStrings #-}

module Day12 (p1, p2) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Foldable
import Data.Maybe
import qualified Data.HashMap.Strict as H
import Data.Scientific as Sci

import Debug.Trace

input = readFile "input/day12.txt"

numbers :: Value -> [Scientific]
numbers (Object o) = H.elems o >>= numbers
numbers (Array a) = toList a >>= numbers
numbers (String _) = []
numbers (Number n) = [n]
numbers x = trace (show x) undefined

p1 = (sum . numbers . Object) <$> ((fromJust . decode . B.pack) <$> input :: IO Object)
-- 119433.0

numbers2 (Object o)
  | "red" `elem` (H.elems o) = []
  | otherwise = H.elems o >>= numbers2
numbers2 (Array a) = toList a >>= numbers2
numbers2 o = numbers o

p2 = (sum . numbers2 . Object) <$> ((fromJust . decode . B.pack) <$> input :: IO Object)
-- 68466.0
