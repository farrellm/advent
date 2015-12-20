module Day8 (p1, p2) where

import Control.Arrow
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer      = P.makeTokenParser emptyDef
decimal    = P.decimal lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

input = readFile "input/day9.txt"

test :: IO String
test = pure "London to Dublin = 464\n\
\London to Belfast = 518\n\
\Dublin to Belfast = 141\n"

data Edge = Edge String String Integer
          deriving Show

edge :: CharParser st Edge
edge = Edge
       <$> (identifier <* string "to ")
       <*> (identifier <* string "= ")
       <*> decimal


parseEdges :: String -> Either ParseError [Edge]
parseEdges = parse (edge `sepEndBy` newline) "(unknown)"

distanceMap :: Foldable t1 => Either t (t1 Edge) -> M.Map (String, String) Integer
distanceMap (Right es) = foldl (\m (Edge c1 c2 d) ->
                                 M.insert (c1, c2) d $
                                 M.insert (c2, c1) d m)
                         M.empty
                         es

distance dMap cs = sum . map (dMap !) $ zip cs (drop 1 cs)

p1 = do
  dMap <- (distanceMap . parseEdges) <$> input
  let cities = nub ((\(a,b) -> [a, b]) =<< M.keys dMap)
  pure . minimum $ map (distance dMap) (permutations cities)
-- 207

p2 = do
  dMap <- (distanceMap . parseEdges) <$> input
  let cities = nub ((\(a,b) -> [a, b]) =<< M.keys dMap)
  pure . maximum $ map (distance dMap) (permutations cities)
-- 804
