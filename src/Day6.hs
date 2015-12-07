{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day6 ( ) where

import BasicPrelude hiding (try)
-- import qualified Data.Set as Set
import qualified Data.Text as T
-- import Data.List.Split
-- import Control.Arrow
import Text.ParserCombinators.Parsec hiding ((<|>))
-- import Data.Vector as V
import Data.Vector.Unboxed.Mutable as M hiding (read)

input = readFile "input/day6.txt"

data Instr = On | Off | Toggle
           deriving (Show)
data Cmd = Cmd Instr (Int, Int) (Int, Int)
           deriving (Show)

cmdFile :: GenParser Char st [Cmd]
cmdFile = endBy cmd eol

cmd :: GenParser Char st Cmd
cmd = do
  i <- instr
  a1 <- string " " *> many digit
  a2 <- string "," *> many digit
  b1 <- string " through " *> many digit
  b2 <- string "," *> many digit
  pure $ Cmd i (read $ T.pack a1, read $ T.pack a2) (read $ T.pack b1, read $ T.pack b2)

instr :: GenParser Char st Instr
instr = (On <$ try (string "turn on")) <|>
        (Off <$ try (string "turn off")) <|>
        (Toggle <$ string "toggle")

eol :: GenParser Char st Char
eol = char '\n'

parseCmds :: String -> Either ParseError [Cmd]
parseCmds = parse cmdFile "(unknown)"

cmds = (either (const []) id . parseCmds . T.unpack) <$> input

size = 5 :: Int

-- grid = M.replicateM size (M.replicate size False)

-- gridRow = M.replicate size False
