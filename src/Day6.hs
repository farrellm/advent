{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day6 ( ) where

import BasicPrelude hiding (try)
-- import qualified Data.Set as Set
import qualified Data.Text as T
-- import Data.List.Split
-- import Control.Arrow
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Data.Vector as V
import Data.Vector as V ((!))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.Primitive

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

applyRect :: (PrimMonad m, Unbox a) =>
             (a -> a) ->
             Vector (M.MVector (PrimState m) a) -> (Int, Int) -> (Int, Int) ->
             m ()
applyRect f grid (x1, y1) (x2, y2) =
  forM_ [y1 .. y2] applyRow
  where applyRow y =
          forM_ [x1 .. x2] (applyCell $ grid ! y)
        applyCell row x = do
          v <- M.read row x
          M.write row x (f v)

cmdRect :: (PrimMonad m) =>
             Instr ->
             Vector (M.MVector (PrimState m) Bool) -> (Int, Int) -> (Int, Int) ->
             m ()
cmdRect (Toggle) = applyRect not
cmdRect (On) = applyRect (const True)
cmdRect (Off) = applyRect (const False)

p1 :: IO Int
p1 = do
  let size = 1000

  cmds <- (either (const []) id . parseCmds . T.unpack) <$> input

  row <- M.replicate size False
  grid <- V.replicateM size (M.clone row)

  forM_ cmds (applyCmd grid)

  countGrid grid

  where applyCmd grid (Cmd ins q1 q2) = cmdRect ins grid q1 q2
        countGrid grid = V.sum <$> V.mapM countRow grid
        countRow row =
          sum <$> mapM ((pure . boolToInt) <=< M.read row) [0..(M.length row - 1)]
        boolToInt True = 1
        boolToInt False = 0
-- 543903

cmdRect2 :: (PrimMonad m) =>
             Instr ->
             Vector (M.MVector (PrimState m) Int) -> (Int, Int) -> (Int, Int) ->
             m ()
cmdRect2 (Toggle) = applyRect (+2)
cmdRect2 (On) = applyRect (+1)
cmdRect2 (Off) = applyRect $ max 0 . (+ (-1))

p2 :: IO Int
p2 = do
  let size = 1000

  cmds <- (either (const []) id . parseCmds . T.unpack) <$> input

  row <- M.replicate size 0
  grid <- V.replicateM size (M.clone row)

  forM_ cmds (applyCmd grid)

  sumGrid grid

  where applyCmd grid (Cmd ins q1 q2) = cmdRect2 ins grid q1 q2
        sumGrid grid = V.sum <$> V.mapM sumRow grid
        sumRow row = sum <$> mapM (M.read row) [0..(M.length row - 1)]
-- 14687245
