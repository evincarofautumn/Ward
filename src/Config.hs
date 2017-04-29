module Config
  ( Config(..)
  , Declaration(..)
  , fromFile
  , fromSource
  , query
  ) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Monoid -- *
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.Map as Map
import qualified Data.Text as Text

-- Grammar of config files:
--
-- <config>          ::= <ws> <declaration>*
-- <declaration>     ::= <name> <modifier>* <description>? <restriction>* ";" <ws>
-- <name>            ::= /[A-Za-z_][0-9A-Za-z_]*/ <ws>
-- <modifier>        ::= "implicit" <ws>
-- <description>     ::= /"([^"]|\\[\\"])*"/ <ws>
-- <restriction>     ::= "->" <ws> <expression> <description>?
-- <expression>      ::= <or-expression>
-- <or-expression>   ::= <and-expression> ("|" <ws> <and-expression>)*
-- <and-expression>  ::= <term> ("&" <ws> <term>)*
-- <term>            ::= <name> | "!" <ws> <term> | "(" <ws> <expression> ")" <ws>
-- <ws>              ::= /([\t\n\r ]|//[^\n]*$)*/

data Declaration = Declaration
  { declImplicit :: !Bool
  , declRestrictions :: [(Expression, Maybe Description)]
  } deriving (Eq, Show)

instance Monoid Declaration where
  mempty = Declaration False mempty
  mappend a b = Declaration
    { declImplicit = declImplicit a || declImplicit b
    , declRestrictions = declRestrictions a <> declRestrictions b
    }

newtype Config = Config (Map PermissionName Declaration)
  deriving (Eq, Show)

instance Monoid Config where
  mempty = Config mempty
  mappend (Config a) (Config b) = Config $ Map.unionWith (<>) a b

query :: PermissionName -> Config -> Maybe Declaration
query p (Config c) = Map.lookup p c

type Description = Text

fromFile :: FilePath -> IO (Either ParseError Config)
fromFile path = fromSource path <$> readFile path

fromSource :: FilePath -> String -> Either ParseError Config
fromSource = parse parser

parser :: Parser Config
parser = Config . Map.fromListWith (<>)
  <$> between silence eof (many declaration)

declaration :: Parser (PermissionName, Declaration)
declaration = (,)
  <$> (permission <* silence)
  <*> (Declaration
    <$> option False (True <$ lexeme (string "implicit"))
    <*> many restriction <* operator ';')

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
