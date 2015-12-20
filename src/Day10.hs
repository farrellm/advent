module Day10 (p1, p2) where

import Control.Arrow
import Control.Monad

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map ((!))

import Text.ParserCombinators.Parsec hiding ((<|>), State)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

lexer      = P.makeTokenParser emptyDef
decimal    = P.decimal lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer

input = 1321131112

test = 1

lookSay :: String -> String
lookSay = join . map (uncurry (++)) . map ((show . length) &&& (pure . head)) . group

p1 = length . (!! 40) . iterate lookSay $ show input
-- 492982

p2 = length . (!! 50) . iterate lookSay $ show input
-- 6989950
