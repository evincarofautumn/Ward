module Config
  ( Config(..)
  , fromFile
  , fromSource
  , query
  ) where

import Check.Permissions (Expression(..), PermissionPresence(..))
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Monoid -- *
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.Map as Map
import qualified Data.Text as Text

-- A config consists of a series of permission declarations:
--
--     permission_name;
--
-- These may include Boolean expressions called "restrictions" that specify
-- constraints on a permission using the OR '|', AND '&', and NOT '!' operators.
--
--     explicit_permission -> implicit_permission;
--     running -> !initializing & !shutting_down;
--     access_globals -> globals_locked | single_threaded;
--
-- A restriction may include a quoted description, to improve error reporting.
--
--     access_globals -> globals_locked | single_threaded
--       "globals must be synchronized, except in single-threaded mode";
--
-- Config files may include comments, which begin with '//' and continue to the
-- end of the line.
--
--     // Just a regular old comment.
--

newtype Config = Config (Map PermissionName [(Expression, Maybe Description)])
  deriving (Eq, Show)

instance Monoid Config where
  mempty = Config mempty
  mappend (Config a) (Config b) = Config $ Map.unionWith (<>) a b

query :: PermissionName -> Config -> Maybe [(Expression, Maybe Description)]
query p (Config c) = Map.lookup p c

type Description = Text

fromFile :: FilePath -> IO (Either ParseError Config)
fromFile path = fromSource path <$> readFile path

fromSource :: FilePath -> String -> Either ParseError Config
fromSource = parse parser

parser :: Parser Config
parser = Config . Map.fromListWith (<>)
  <$> between silence eof (many declaration)

declaration :: Parser (PermissionName, [(Expression, Maybe Description)])
declaration = (,)
  <$> (permission <* silence)
  <*> (toList <$> optionMaybe restriction) <* operator ';'

restriction :: Parser (Expression, Maybe Description)
restriction = lexeme (string "->")
  *> ((,) <$> expression <*> optionMaybe description)

expression :: Parser Expression
expression = orExpression
  where
    orExpression = foldr1 Or <$> andExpression `sepBy1` operator '|'
    andExpression = foldr1 And <$> term `sepBy1` operator '&'
    term = choice
      [ Context . Has <$> permission
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

permission :: Parser PermissionName
permission = PermissionName . Text.pack <$> lexeme ((:) <$> first <*> many rest)
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
