{-# LANGUAGE OverloadedStrings #-}

module Config
  ( fromFile
  , fromSource
  ) where

import Control.Monad (mzero, void)
import Data.Either
import Data.Monoid -- *
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
      [ Context . Has NoReason <$> permission
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
