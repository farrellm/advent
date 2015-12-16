{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day7 ( ) where

import BasicPrelude hiding (try)

import Data.Bits
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
type Term = Either String Word16
data Value = Bin BinGate Term Term |
             Uni UniGate Term
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

value :: CharParser st Term
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
calculate l = gets (fromMaybe undefined . M.lookup l) >>= calcGate
  where toInt = fromInteger . toInteger

        calcTerm :: Term -> State Lookup Word16
        calcTerm (Right x) = pure x
        calcTerm (Left s) = do
          v <- gets (fromMaybe undefined . M.lookup s) >>= calcGate
          modify (M.insert s (Uni Id (Right v)))
          pure v

        calcGate :: Value -> State Lookup Word16
        calcGate (Uni Id t) = calcTerm t
        calcGate (Uni Not t) = complement <$> calcTerm t
        calcGate (Bin And a b) = (.&.) <$> calcTerm a <*> calcTerm b
        calcGate (Bin Or a b) = (.|.) <$> calcTerm a <*> calcTerm b
        calcGate (Bin LShift a b) = shiftL <$> calcTerm a <*> (toInt <$> calcTerm b)
        calcGate (Bin RShift a b) = shiftR <$> calcTerm a <*> (toInt <$> calcTerm b)

p1 :: IO Word16
p1 = do
  circuits <- (either (const []) id . parseCircuits . T.unpack) <$> input

  let gates = foldl (\m (Circuit v k) -> M.insert k v m) M.empty circuits
  pure . fst $ (runState $ calculate "a") gates
-- 956

p2 :: IO Word16
p2 = do
  circuits <- (either (const []) id . parseCircuits . T.unpack) <$> input

  let gates = foldl (\m (Circuit v k) -> M.insert k v m) M.empty circuits
  pure . fst $ (runState $ calculate "a") (M.insert "b" (Uni Id (Right 956)) gates)
-- 40149
