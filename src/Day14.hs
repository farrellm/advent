{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day14 (p1, p2) where

import BasicPrelude hiding (try)
import qualified Data.Map as M
import qualified Data.Text as T
-- import Data.List.Split
-- import Control.Arrow
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer      = P.makeTokenParser emptyDef
decimal    = P.decimal lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

input :: IO Text
input = readFile "input/day14.txt"

data ReinState = Flying Integer | Resting Integer
               deriving (Show, Eq)
data Reindeer = Reindeer ReinState Integer String Integer Integer Integer
              deriving (Show, Eq)
instance Ord(Reindeer) where
  (Reindeer _ d1 _ _ _ _) <= (Reindeer _ d2 _ _ _ _) = d1 <= d2

reindeerFile :: CharParser st [Reindeer]
reindeerFile = many reindeer

reindeer :: CharParser st Reindeer
reindeer = Reindeer (Flying 1) 0
           <$> (identifier <* string "can fly ")
           <*> (decimal <* string " km/s for ")
           <*> (decimal <* string " seconds, but then must rest for ")
           <*> (decimal <* string " seconds." <* whiteSpace)


parseReindeer :: String -> Either ParseError [Reindeer]
parseReindeer = parse reindeerFile "(unknown)"

step :: Reindeer -> Reindeer

step (Reindeer (Flying t) d n speed flyTime restTime)
  | t == flyTime = Reindeer (Resting 1) (d + speed) n speed flyTime restTime
  | otherwise    = Reindeer (Flying $ t + 1) (d + speed) n speed flyTime restTime

step (Reindeer (Resting t) d n speed flyTime restTime)
  | t == restTime = Reindeer (Flying 1) d n speed flyTime restTime
  | otherwise     = Reindeer (Resting $ t + 1) d n speed flyTime restTime

test :: IO Text
test = pure "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
\Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

distance :: Reindeer -> Integer
distance (Reindeer _ d _ _ _ _) = d

name :: Reindeer -> String
name (Reindeer _ _ n _ _ _) = n

p1 :: IO Integer
p1 = (maximum . map distance .
      (!! 2503) .
      (iterate $ fmap step) .
      either (const []) id . parseReindeer . T.unpack) <$> input
-- 2655

type Scores = M.Map String Integer

initScores :: [Reindeer] -> Scores
initScores = foldl (\m r -> M.insert (name r) 0 m) M.empty

updateScores :: Scores -> [Reindeer] -> Scores
updateScores sc rs =
  let winning = distance $ maximum rs
  in foldl (flip $ M.adjust (+1))
     sc .
     map name $ filter ((winning ==) . distance) rs

p2 :: IO (String, Integer)
p2 = do
  rs <- (either (const []) id . parseReindeer . T.unpack) <$> input
  let steps = drop 1 $ (iterate $ fmap step) rs
  let winners = (map M.findMax) $ scanl updateScores (initScores rs) steps

  pure (winners !! 2503)
-- ("Vixen",1059)
