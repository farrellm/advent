module Day3 () where

import Data.List
import Control.Arrow

input = readFile "input/day3.txt"

move (x, y) c = case c of
  '^' -> (x, y+1)
  'v' -> (x, y-1)
  '<' -> (x-1, y)
  '>' -> (x+1, y)

p1 = (length . nub . scanl move (0,0)) <$> input

split (a : b : rs) =
  let (as, bs) = split rs
  in (a : as, b : bs)
split a = (a, [])

moves = nub . scanl move (0,0)

p2 = (length . nub . (\(a, b) -> a `mappend` b) . (moves *** moves) . split) <$> input
