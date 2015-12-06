module Day1 ( ) where

input = readFile "input/day1.txt"

foo '(' = 1
foo ')' = -1

p1 = sum . map foo <$> input
-- 138

zs = zip [1..] . scanl (+) 0 . map foo <$> input

p2 = last . takeWhile (\(i, x) -> x >= 0) <$> zs
-- (1771,0)
