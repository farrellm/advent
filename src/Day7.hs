{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day7 ( ) where

import BasicPrelude hiding (try)

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Text as T
import Data.Word
import Text.ParserCombinators.Parsec hiding ((<|>), State)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Monad.State

lexer      = P.makeTokenParser emptyDef
decimal    = P.decimal lexer
identifier = P.identifier lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer

input = readFile "input/day7.txt"

data BinGate = And | Or | LShift | RShift
          deriving (Show)
data UniGate = Id | Not
          deriving (Show)
data Value = Bin BinGate (Either String Word16) (Either String Word16) |
             Uni UniGate (Either String Word16)
             deriving (Show)
data Circuit = Circuit Value String
             deriving (Show)

circuitFile :: CharParser st [Circuit]
circuitFile = many circuit

circuit :: CharParser st Circuit
circuit =
  try (Circuit <$> (flip Bin <$> value <*> binGate <*> value) <*> output) <|>
  try (Circuit <$> (Uni Id <$> value) <*> output) <|>
  Circuit <$> (Uni <$> uniGate <*> value) <*> output

value :: CharParser st (Either String Word16)
value = try ((Right . fromInteger) <$> decimal <* whiteSpace) <|>
        (Left <$> identifier)

output = string "->" *> whiteSpace *> identifier

binGate :: CharParser st BinGate
binGate = ((And <$ string "AND") <|>
           (Or <$ string "OR") <|>
           (RShift <$ string "RSHIFT") <|>
           (LShift <$ string "LSHIFT")) <* whiteSpace

uniGate :: CharParser st UniGate
uniGate = Not <$ string "NOT" <* whiteSpace

parseCircuits :: String -> Either ParseError [Circuit]
parseCircuits = parse circuitFile "(unknown)"

test :: IO Text
test = pure "123 -> x\n\
\456 -> y\n\
\x AND y -> d\n\
\x OR y -> e\n\
\x LSHIFT 2 -> f\n\
\y RSHIFT 2 -> g\n\
\NOT x -> h\n\
\NOT y -> i"

type Lookup = M.Map String Value

calculate :: String -> State Lookup Word16
calculate s = do
  m <- get
  let v = m ! s
  pure 0

-- p1 :: IO [Circuit]
p1 = do
  circuits <- (either (const []) id . parseCircuits . T.unpack) <$> test

  let gates = foldl (\m (Circuit v k) -> M.insert k v m) M.empty circuits



  pure gates
