{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Day12 (p1, p2) where

import Control.Arrow
import Control.Monad

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Maybe
import GHC.Generics
import qualified Data.HashMap.Strict as H

input = readFile "input/day12.txt"

test :: IO String
test = pure ""

numbers :: Value -> [Integer]
numbers obj = []

p1 = (H.keys) <$> ((fromJust . decode . B.pack) <$> input :: IO Object)
-- p1 = 0

p2 = 0
