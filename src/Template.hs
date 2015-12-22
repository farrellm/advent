module Day8 (p1, p2) where

import Control.Arrow
import Control.Monad

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

import Text.ParserCombinators.Parsec hiding ((<|>), State)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer      = P.makeTokenParser emptyDef
decimal    = P.decimal lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

input = readFile "input/day9.txt"

test :: IO String
test = pure ""

p1 = 0

p2 = 0
