module Config
  ( Config(..)
  , Restriction(..)
  , fromFile
  , fromSource
  , query
  ) where

import Control.Monad (void)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Monoid -- *
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.Map as Map
import qualified Data.Text as Text

newtype Config = Config (Map Permission [(Restriction, Maybe Description)])
  deriving (Eq, Show)

instance Monoid Config where
  mempty = Config mempty
  mappend (Config a) (Config b) = Config $ Map.unionWith (<>) a b

query :: Permission -> Config -> Maybe [(Restriction, Maybe Description)]
query p (Config c) = Map.lookup p c

data Restriction
  = !Restriction :& !Restriction
  | !Restriction :| !Restriction
  | Not !Restriction
  | Literal !Permission
  deriving (Eq, Show)

instance IsString Restriction where
  fromString = Literal . fromString

infixr 3 :&
infixr 2 :|

type Description = Text

fromFile :: FilePath -> IO (Either ParseError Config)
fromFile path = fromSource path <$> readFile path

fromSource :: FilePath -> String -> Either ParseError Config
fromSource = parse parser

parser :: Parser Config
parser = Config . Map.fromListWith (<>)
  <$> between silence eof (many declaration)

declaration :: Parser (Permission, [(Restriction, Maybe Description)])
declaration = (,)
  <$> (permission <* silence)
  <*> (toList <$> optionMaybe restriction) <* operator ';'

restriction :: Parser (Restriction, Maybe Description)
restriction = lexeme (string "->")
  *> ((,) <$> expression <*> optionMaybe description)

expression :: Parser Restriction
expression = orExpression
  where
    orExpression = foldr1 (:|) <$> andExpression `sepBy1` operator '|'
    andExpression = foldr1 (:&) <$> term `sepBy1` operator '&'
    term = choice
      [ Literal <$> permission
      , Not <$> (operator '!' *> term)
      , parenthesized expression
      ]
    parenthesized = between (operator '(') (operator ')')

description :: Parser Description
description = fmap Text.pack $ lexeme $ quoted $ many $ character <|> escape
  where
    character = noneOf "\\\""
    escape = char '\\' *> choice
      [ '\\' <$ char '\\'
      , '"' <$ char '"'
      ]
    quoted = between (char '"') (char '"')

permission :: Parser Permission
permission = Permission . Text.pack <$> lexeme ((:) <$> first <*> many rest)
  where
    first = letter <|> char '_'
    rest = alphaNum <|> char '_'

silence :: Parser ()
silence = void $ many $ choice
  [ (:[]) <$> oneOf "\t\n\r "
  , between (string "//") (void (char '\n') <|> eof) $ many $ noneOf "\n"
  ]

operator :: Char -> Parser ()
operator = void . lexeme . char

lexeme :: Parser a -> Parser a
lexeme = (<* silence)
