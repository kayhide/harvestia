{-# LANGUAGE DerivingStrategies #-}
module Renkon.Inflector
  where

import ClassyPrelude hiding (many, some, try)

import Control.Lens ((%~), (&), _head)
import qualified Data.Char as Char
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


newtype NormalizedWord = NormalizedWord Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)


class Camelcase a where
  camelcase :: a -> Text

class Snakecase a where
  snakecase :: a -> Text

class Cebabcase a where
  cebabcase :: a -> Text


instance Camelcase NormalizedWord where
  camelcase (NormalizedWord x) = x

instance Camelcase [NormalizedWord] where
  camelcase = foldr (\x acc -> camelcase x <> (acc & _head %~ Char.toUpper)) ""

instance Camelcase Text where
  camelcase = camelcase . wordize


instance Snakecase NormalizedWord where
  snakecase (NormalizedWord x) = x

instance Snakecase [NormalizedWord] where
  snakecase = intercalate "_" . fmap snakecase

instance Snakecase Text where
  snakecase = snakecase . wordize


instance Cebabcase NormalizedWord where
  cebabcase (NormalizedWord x) = x

instance Cebabcase [NormalizedWord] where
  cebabcase = intercalate "-" . fmap cebabcase

instance Cebabcase Text where
  cebabcase = cebabcase . wordize


type Parser' = Parsec Void Text

camelParser :: Parser' [NormalizedWord]
camelParser = do
  x <- (<>) <$> many (oneOf uppers') <*> many (oneOf $ lowers' <> numbers')
  xs <- many $ do
    (<>) <$> some (oneOf uppers') <*> many (oneOf $ lowers' <> numbers')
  eof
  pure $ NormalizedWord . toLower . pack <$> (x : xs)
  where
    lowers' :: [Char]
    lowers' = ['a' .. 'z']

    uppers' :: [Char]
    uppers' = ['A' .. 'Z']

    numbers' :: [Char]
    numbers' = ['0' .. '9']

wordsParser :: Parser' [NormalizedWord]
wordsParser = do
  xs <- some (oneOf chars') `sepBy` some (noneOf chars')
  eof
  pure $ NormalizedWord . toLower . pack <$> xs
  where
    chars' :: [Char]
    chars' = ['0' .. '9'] <> ['a' .. 'z'] <> ['A' .. 'Z']

wordize :: Text -> [NormalizedWord]
wordize = either (const []) id . parse (try camelParser <|> wordsParser) ""


camelcaseWithHead :: Text -> Text
camelcaseWithHead t = camelcase t & _head %~ Char.toUpper
