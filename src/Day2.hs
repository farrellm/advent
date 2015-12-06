module Day2 ( ) where

import Data.List.Split

sideAreas l w h = [l * w, w * h, h* l]

area [l, w, h] = 2 * (sum $ sideAreas l w h) + (minimum $ sideAreas l w h)

input = readFile "input/day2"

toSides = map ((map read) . splitOn "x") . lines :: String -> [[Int]]

p1 = sum . map area . toSides <$> input
-- 1588178

sidePerims l w h = map (2*) [l+w, l+h, w+h]
bow l w h = l * w * h

ribbon [l, w, h] = (minimum $ sidePerims l w h) + bow l w h

p2 = sum . map ribbon . toSides <$> input
-- 3783758
