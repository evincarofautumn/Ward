{-# language FlexibleInstances, OverloadedStrings, ViewPatterns, GeneralizedNewtypeDeriving #-}
module ParseCallMap (fromFile, fromSource, CallMapParseError) where

import Types

import Control.Monad (void)
import Data.Char (digitToInt)
import Data.Foldable (asum)
import Data.String (IsString)

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Text.Lazy

import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Data.Text.Encoding.Error as TextL

import Language.C.Data.Ident (Ident)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Data.Name (Name)
import Language.C.Data.Node (NodeInfo)
import qualified Language.C.Data.Node as CNode
import qualified Language.C.Data.Position as CPos

-- Grammar of graph files (assumes UTF8 encoding):
--
-- The file consists of S-expressions (where the head of each non-empty
-- subexpression must be an identifier) given by the following grammar,
-- interpreted by the rules below.  Everything is case-sensitive.  Integer
-- literals that do not fit in a haskell 'Int' type are silently (and
-- erroneously) wrapped.
--
--  <graph>      ::= <ws> <form>*
--  <form>       ::= "(" <ws> <identifier> <exp>* ")" <ws> | "(" <ws> ")" <ws>
--  <exp>        ::= <value> | <form>
--  <value>      ::= <identifier> | <string_lit> | <int_lit>
--  <ws>         ::= /([\t\n\r ]|;[^\n]*$)*/
--  <string_lit> ::= /"([^\\"]|\\[\\"]|)*"/ <ws>
--  <int_lit>    ::= /[-+]?/ <ws> /0|([1-9][0-9]*)/ <ws>
--  <identifier> ::= /[_:isAlpha:][_:isAlphaNum:]*/ <ws>
--
-- The graph file consists of a single callmapForm.
--
-- callmapForm          ::= ( callmap <functionForm>* )
-- functionForm         ::= ( function <identForm> <nodeForm> <actionsForm> <calltreeForm> )
-- actionsForm          ::= ( actions <permissionActionForm>* )
-- permissionActionForm ::= ( [need|use|grant|revoke|deny|waive] <identifier> )
-- calltreeForm         ::= ( calltree <subcallForm>* )
-- subcallForm          ::= ( call <identForm> )
--                       |  ( choice <calltreeForm> <calltreeForm> )
--                       |  ( seq <subcallForm>* )
-- identForm            ::= ( ident <string_lit> <nodeForm> )
-- nodeForm             ::= noNode
--                       |  ( node <int> <cspanForm> )
--                       |  ( onlypos <cspanForm> )
-- cspanForm            ::= noPos
--                       |  builtin
--                       |  internal
--                       |  ( span <sourceForm> <int_lit> <sourceForm> )
-- sourceForm           ::= ( source <string_lit> <int_lit> <int_lit> <int_lit> )
--


-- | An identifier
newtype Sidentifier = Sidentifier { sidentText :: Text.Text }
  deriving (Show, Eq, IsString)

-- | Parse the given callmap graph file.  Assumes UTF8 encoding (will use
-- U+FFFD replacement character on UTF8 decoding errors).
fromFile :: FilePath -> IO (Either CallMapParseError CallMap)
fromFile path = do
  bs <- BSL.readFile path
  let txt = TextL.decodeUtf8With TextL.lenientDecode bs
  return (fromSource path txt)


-- | @fromSource path txt@ parses @txt@ and returns the resulting 'CallMap'.
-- (The @path@ is used in error messages)
fromSource :: FilePath -> TextL.Text -> Either CallMapParseError CallMap
fromSource = parse parser

parser :: Parser CallMap
parser = between ws eof callmapForm

callmapForm :: Parser CallMap
callmapForm = theForm "callmap" (Map.fromList <$> many functionForm)

type CallMapItem = (NodeInfo, CallTree Ident, PermissionActionSet)

functionForm :: Parser (Ident, CallMapItem)
functionForm =
  let
    fnbody = (,) <$> identForm <*> item
    item = mkItem <$> nodeForm <*> actionsForm <*> calltreeForm
  in theForm "function" fnbody
  where
    mkItem node actions tree = (node, tree, actions)

actionsForm :: Parser PermissionActionSet
actionsForm = theForm "actions" (HashSet.fromList <$> many permissionActionForm)

permissionActionForm :: Parser PermissionAction
permissionActionForm =
  aForm (keyword <*> (permissionName <$> identifierLiteral))
  where
    keyword = asum (map kw actions)
    kw
      :: (Sidentifier, (PermissionName -> PermissionAction))
      -> Parser (PermissionName -> PermissionAction)
    kw (ident,f) = f <$ theIdentifier ident
    actions :: [(Sidentifier, PermissionName -> PermissionAction)]
    actions =
      [ ("need", Need)
      , ("use", Use)
      , ("grant", Grant)
      , ("revoke", Revoke)
      , ("deny", Deny)
      , ("waive", Waive)
      ]
    permissionName :: Spanning Sidentifier -> PermissionName
    permissionName = PermissionName . sidentText . extractSpan

calltreeForm :: Parser (CallTree Ident)
calltreeForm = theForm "calltree" seqForest

seqForest :: Parser (CallTree Ident)
seqForest = joinForest <$> many subcallForm
  where
    joinForest :: [CallTree a] -> CallTree a
    joinForest = foldr combineSeq Nop
    combineSeq :: CallTree a -> CallTree a -> CallTree a
    combineSeq Nop a = a
    combineSeq a Nop = a
    combineSeq a b = Sequence a b

subcallForm :: Parser (CallTree Ident)
subcallForm =
  aForm (seqSubForm <|> choiceSubForm <|> callSubForm) <?> "call, choice or seq subform"
  where
    seqSubForm :: Parser (CallTree Ident)
    seqSubForm = theIdentifier "seq" *> seqForest

    callSubForm :: Parser (CallTree Ident)
    callSubForm = Call <$ theIdentifier "call" <*> identForm

    choiceSubForm :: Parser (CallTree Ident)
    choiceSubForm = Choice <$ theIdentifier "choice" <*> subcallForm <*> subcallForm

identForm :: Parser Ident
identForm =
  theForm "ident" (mkCIdent <$> (extractSpan <$> stringLiteral) <*> nodeForm)
  where
    mkCIdent s ni = CIdent.internalIdentAt (CPos.posOf ni) s -- FIXME: internalIdentAt isn't the right constructor

nodeForm :: Parser CNode.NodeInfo
nodeForm =
  (CNode.undefNode <$ theIdentifier "noNode")
  <|> aForm (nodeinfoForm <|> onlyposForm)

data CSpan = NoPosCSpan | BuiltinPosCSpan | InternalPosCSpan | CSpanPosLength CPos.Position CPos.PosLength

mkCNodeInfo :: Maybe Name -> CSpan -> CNode.NodeInfo
mkCNodeInfo mnm sp =
  case sp of
    NoPosCSpan -> CNode.mkNodeInfoOnlyPos CPos.nopos
    BuiltinPosCSpan -> CNode.mkNodeInfoOnlyPos CPos.builtinPos
    InternalPosCSpan -> CNode.mkNodeInfoOnlyPos CPos.internalPos
    CSpanPosLength pos poslen -> makeSpan pos poslen
  where
    makeSpan =
      case mnm of
        Just nm -> \ pstart pend -> CNode.mkNodeInfo' pstart pend nm
        Nothing -> CNode.mkNodeInfoPosLen



nodeinfoForm :: Parser CNode.NodeInfo
nodeinfoForm = mkCNodeInfo <$ theIdentifier "node" <*> name <*> cspanForm
  where
    name :: Parser (Maybe Name)
    name = (Just . toEnum . extractSpan) <$> intLiteral

onlyposForm :: Parser CNode.NodeInfo
onlyposForm = mkCNodeInfo Nothing <$ theIdentifier "onlypos" <*> cspanForm

cspanForm :: Parser CSpan
cspanForm =
  let
    nopos = NoPosCSpan <$ theIdentifier "noPos"
    builtin = BuiltinPosCSpan <$ theIdentifier "builtin"
    internal = InternalPosCSpan <$ theIdentifier "internal"
    spanForm = theForm "span" spanBody
    spanBody = mkSpanPos <$> sourceForm <*> len <*> sourceForm
    len = extractSpan <$> intLiteral
  in nopos <|> builtin <|> internal <|> spanForm
  where
    mkSpanPos pstart len pend = CSpanPosLength pstart (pend, len)

sourceForm :: Parser CPos.Position
sourceForm =
  let
    sourceBody = mkPos <$> path <*> intlit <*> intlit <*> intlit
    path = extractSpan <$> stringLiteral
    intlit = extractSpan <$> intLiteral
  in theForm "source" sourceBody
  where
    mkPos path offset row column = CPos.position offset path row column Nothing

------------------------------------------------------------
-- Parsing S-expression files
------------------------------------------------------------

-- | A 'Span' is a contiguous region between two source positions.
-- The first position in a span is the start the second is the end
-- (we will maintain, but not check, the invariant that the start comes
-- before the end in the file)
type Span = (SourcePos, SourcePos)

-- | A @Spanning a@ is a pair of a 'Span' and a value
type Spanning a = (Span, a)

-- | @Spanning a@ is a functor and this is its 'fmap'
fmapSpan :: (a -> b) -> Spanning a -> Spanning b
fmapSpan = fmap


-- | @Spanning a@ is a comonad with 'extractSpan' the counit and 'extendSpan'
-- its composition operations.
extractSpan :: Spanning a -> a
extractSpan = snd

-- unused
-- | @Spanning a@ is a comonad with 'extendSpan' its composition operation and
-- 'extractSpan' the counit.
-- extendSpan :: (Spanning a -> b) -> Spanning a -> Spanning b
-- extendSpan f w = (fst w, f w)


theForm :: Sidentifier -> Parser a -> Parser a
theForm ident rest = aForm (theIdentifier ident *> rest)

aForm :: Parser a -> Parser a
aForm = between lparen rparen

theIdentifier :: Sidentifier -> Parser (Spanning Sidentifier)
theIdentifier ident =
  let s = sidentText ident
  in lexeme $ try $ do
     _ <- string (Text.unpack s)
     notFollowedBy (alphaNum <|> char '_') <?> ("end of " ++ show s)
     return ident

identifierLiteral :: Parser (Spanning Sidentifier)
identifierLiteral = lexeme (mkIdentifier <$> identChars) <?> "identifier"
  where
    mkIdentifier = Sidentifier . Text.pack
    identChars :: Parser String
    identChars = ((:) <$> first <*> many rest)
    first = letter <|> char '_'
    rest = alphaNum <|> char '_'

intLiteral :: Parser (Spanning Int)
intLiteral = fmapSpan fromInteger <$> integer

stringLiteral :: Parser (Spanning String)
stringLiteral = lexeme $ quoted $ many $ character <|> escape
  where
    character = noneOf "\\\""
    escape = char '\\' *> choice
      [ '\\' <$ char '\\'
      , '"' <$ char '"'
      ]
    quoted = between (char '"') (char '"')

lparen :: Parser (Spanning ())
lparen = void <$> lexeme (char '(')

rparen :: Parser (Spanning ())
rparen = void <$> lexeme (char ')')

spanning :: Parser a -> Parser (Spanning a)
spanning p = mkSpanning <$> getPosition <*> p <*> getPosition
  where
    mkSpanning start a end = ((start, end), a)

-- tokenizer definitions borrowed, somewhat, from Text.Parsec.Token

lexeme :: Parser a -> Parser (Spanning a)
lexeme = lexeme' . spanning -- grab position before consuming whitespace

lexeme' :: Parser a -> Parser a
lexeme' = (<* ws)

ws :: Parser ()
ws = flip label "whitespace or comment" $ skipMany $ choice
  [ void $ oneOf "\t\n\r "
  , between (char ';') (void (char '\n') <|> eof) $ skipMany $ noneOf "\n"
  ]

integer :: Parser (Spanning Integer)
integer = lexeme int <?> "integer"

sign :: Parser (Integer -> Integer)
sign =
  choice [negate <$ char '-', id <$ char '+', pure id]

int, nat, zeroNumber, decimal :: Parser Integer

int = do
  f <- lexeme' sign
  n <- nat
  return (f n)

nat = zeroNumber <|> decimal

zeroNumber =
  (char '0' *> pure 0)
  <?> "0"

decimal = number 10 digit

number :: Integer -> Parser Char-> Parser Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

