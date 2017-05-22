{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , Declaration(..)
  , fromFile
  , fromSource
  , query
  ) where

import Control.Monad (mzero, void)
import Data.Either
import Data.Map (Map)
import Data.Monoid -- *
import Data.Text (Text)
import Data.These
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.Map as Map
import qualified Data.Text as Text

-- Grammar of config files:
--
-- <config>          ::= <ws> (<directive> | <declaration>)*
-- <directive>       ::= ".enforce" <ws> <enforcement> ";" <ws>
-- <enforcement>     ::= <string> | <name> | <string> <name>
-- <declaration>     ::= <name> <modifier>* <description>? <restriction>* ";" <ws>
-- <modifier>        ::= "implicit" <ws>
-- <description>     ::= <string>
-- <restriction>     ::= "->" <ws> <expression> <description>?
-- <expression>      ::= <or-expression>
-- <or-expression>   ::= <and-expression> ("|" <ws> <and-expression>)*
-- <and-expression>  ::= <term> ("&" <ws> <term>)*
-- <term>            ::= <name> | "!" <ws> <term> | "(" <ws> <expression> ")" <ws>
-- <name>            ::= /[A-Za-z_][0-9A-Za-z_]*/ <ws>
-- <string>          ::= /"([^"]|\\[\\"])*"/ <ws>
-- <ws>              ::= /([\t\n\r ]|//[^\n]*$)*/

data Declaration = Declaration
  { declImplicit :: !Bool
  , declDescription :: !(Maybe Description)
  , declRestrictions :: [(Expression, Maybe Description)]
  } deriving (Eq, Show)

instance Monoid Declaration where
  mempty = Declaration False Nothing mempty
  mappend a b = Declaration
    { declImplicit = declImplicit a || declImplicit b
    , declDescription = case (declDescription a, declDescription b) of
      (Just da, Just db) -> Just (da <> "; " <> db)
      (da@Just{}, Nothing) -> da
      (Nothing, db@Just{}) -> db
      _ -> Nothing
    , declRestrictions = declRestrictions a <> declRestrictions b
    }

data Config = Config
  { configDeclarations :: !(Map PermissionName Declaration)
  , configEnforcements :: [Enforcement]
  } deriving (Eq, Show)

instance Monoid Config where
  mempty = Config mempty mempty
  mappend (Config declA enfA) (Config declB enfB) = Config
    (Map.unionWith (<>) declA declB)
    (enfA <> enfB)

query :: PermissionName -> Config -> Maybe Declaration
query p (Config c _) = Map.lookup p c

type Description = Text

fromFile :: FilePath -> IO (Either ParseError Config)
fromFile path = fromSource path <$> readFile path

fromSource :: FilePath -> String -> Either ParseError Config
fromSource = parse parser

parser :: Parser Config
parser = do
  (declarations, enforcements) <- partitionEithers
    <$> between silence eof (many (Left <$> declaration <|> Right <$> enforcement))
  pure Config
    { configDeclarations = Map.fromListWith (<>) declarations
    , configEnforcements = enforcements
    }

enforcement :: Parser Enforcement
enforcement = lexeme (string ".enforce") *> do
  mPath <- optionMaybe stringLiteral
  mName <- optionMaybe name
  operator ';'
  case (mPath, mName) of
    (Just path, Just function) -> pure $ These path $ Text.pack function
    (Just path, Nothing) -> pure $ This path
    (Nothing, Just function) -> pure $ That $ Text.pack function
    _ -> mzero

declaration :: Parser (PermissionName, Declaration)
declaration = (,)
  <$> (permission <* silence)
  <*> (Declaration
    <$> option False (True <$ lexeme (string "implicit"))
    <*> optionMaybe description
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
description = Text.pack <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = lexeme $ quoted $ many $ character <|> escape
  where
    character = noneOf "\\\""
    escape = char '\\' *> choice
      [ '\\' <$ char '\\'
      , '"' <$ char '"'
      ]
    quoted = between (char '"') (char '"')

permission :: Parser PermissionName
permission = PermissionName . Text.pack <$> name

name :: Parser String
name = lexeme ((:) <$> first <*> many rest)
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
