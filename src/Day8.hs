{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Day8 ( ) where

import BasicPrelude hiding (try)

import Control.Arrow
import Data.Char
import Data.Text (pack, unpack)
import Text.ParserCombinators.Parsec hiding ((<|>), State)

input = readFile "input/day8.txt"

stringLine :: CharParser st String
stringLine = char '"' *> many uniChar <* char '"'

uniChar :: CharParser st Char
uniChar = (char '\\' *> (char '"' <|>
                         char '\\' <|>
                         (char 'x' *> octChar))) <|>
          alphaNum

octChar :: CharParser st Char
octChar = (chr . read . pack . ("0x" <>)) <$> count 2 hexDigit

stringFile :: CharParser st [String]
stringFile = stringLine `endBy` newline

anyFile :: CharParser st [String]
anyFile = many (alphaNum <|> char '\\' <|> char '"') `sepBy` newline

parseStrings :: String -> Either ParseError [String]
parseStrings = parse stringFile "(unknown)"

parseAny :: String -> Either ParseError [String]
parseAny = parse anyFile "(unknown)"

test :: IO Text
test = pure "\"\"\n\
\\"abc\"\n\
\\"aaa\\\"aaa\"\n\
\\"\\x27\"\n"

p1 = (uncurry (-)) <$>
     (ln *** ln) <$>
     (parseAny &&& parseStrings) <$>
     (textToString <$> input)
  where ln = length . mconcat . either (const []) id
-- 1333

p2 = (uncurry (-)) <$>
     ((sum . (map (length . unpack) . map show . lines))
      &&& (ln . parseAny . textToString)) <$> input
  where ln = length . mconcat . either (const []) id
-- 2046
