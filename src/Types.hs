{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Types where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad.IO.Class (MonadIO(..))
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.These
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Node (NodeInfo(..))
import Language.C.Data.Position (posFile, posRow)
import Language.C.Syntax.AST -- *
import qualified Data.Text as Text

type FunctionName = Text

type NameMap = Map Ident (NodeInfo, Maybe CFunDef, PermissionActionSet)

type CallMap = Map Ident (NodeInfo, CallTree Ident, PermissionActionSet)

data CallTree a
  = Sequence !(CallTree a) !(CallTree a)
  | Choice !(CallTree a) !(CallTree a)
  | Call !a
  | Nop
  deriving (Foldable, Functor, Traversable)

instance (Show a) => Show (CallTree a) where
  showsPrec p = \ case
    Sequence a b -> showParen (p > sequencePrec)
      $ showsPrec sequencePrec a . showString " ; " . showsPrec sequencePrec b
    Choice a b -> showParen (p > choicePrec)
      $ showsPrec choicePrec a . showString " | " . showsPrec choicePrec b
    Call ident -> shows ident
    Nop -> id
    where
      sequencePrec = 1
      choicePrec = 0

callTreeBreadth :: CallTree a -> Int
callTreeBreadth (Sequence a b) = callTreeBreadth a + callTreeBreadth b
callTreeBreadth Choice{} = 1  -- Not sure if this is correct.
callTreeBreadth Call{} = 1
callTreeBreadth Nop = 0

data Entry
  = Note !NodeInfo !Text
  | Warning !NodeInfo !Text
  | Error !NodeInfo !Text

posPrefix :: NodeInfo -> String
posPrefix (OnlyPos pos _) = concat
  [ posFile pos
  , ":"
  , show $ posRow pos
  ]
posPrefix (NodeInfo pos _ _) = concat
  [ posFile pos
  , ":"
  , show $ posRow pos
  ]

newtype Logger a = Logger { runLogger :: Chan (Maybe Entry) -> IO a }

instance Functor Logger where
  fmap f (Logger g) = Logger $ fmap f . g

instance Applicative Logger where
  pure = Logger . const . return
  Logger f <*> Logger g = Logger (\ entries -> f entries <*> g entries)

instance Monad Logger where
  Logger f >>= g = Logger
    $ \ entries -> flip runLogger entries . g =<< f entries

instance MonadIO Logger where
  liftIO = Logger . const

record :: Bool -> Entry -> Logger ()
record False _ = return ()
record True entry = Logger $ \ entries -> writeChan entries $ Just entry

endLog :: Logger ()
endLog = Logger $ \ entries -> writeChan entries Nothing

data OutputMode
  = CompilerOutput
  | HtmlOutput
  deriving (Eq)

format :: OutputMode -> Entry -> String

format CompilerOutput entry = case entry of
  Note p t -> concat [posPrefix p, ": note: ", Text.unpack t]
  Warning p t -> concat [posPrefix p, ": warning: ", Text.unpack t]
  Error p t -> concat [posPrefix p, ": error: ", Text.unpack t]

-- TODO: Convert position to URL for hyperlinked output.
format HtmlOutput entry = case entry of
  Note p t -> row "note" t
  Warning p t -> row "warning" t
  Error p t -> row "error" t
  where
    row category text = concat
      ["<li class='", category, "'>", Text.unpack text, "</li>"]

formatHeader :: OutputMode -> String
formatHeader CompilerOutput = ""
formatHeader HtmlOutput = "\
  \<html>\n\
  \<head>\n\
  \<title>Ward Report</title>\n\
  \</head>\n\
  \<body>\n\
  \<ul>\n\
  \\&"

formatFooter :: OutputMode -> String -> String
formatFooter CompilerOutput extra = extra
formatFooter HtmlOutput extra = "\
  \</ul>\n\
  \\&" <> extra <> "\n\
  \</body>\n\
  \</html>\n\
  \\&"

partitionEntries
  :: [Entry]
  -> ([(NodeInfo, Text)], [(NodeInfo, Text)], [(NodeInfo, Text)])
partitionEntries = go mempty
  where
    go (ns, ws, es) = \ case
      Note a b : rest -> go ((a, b) : ns, ws, es) rest
      Warning a b : rest -> go (ns, (a, b) : ws, es) rest
      Error a b : rest -> go (ns, ws, (a, b) : es) rest
      [] -> (reverse ns, reverse ws, reverse es)

newtype PermissionName = PermissionName Text
  deriving (Eq, Hashable, IsString, Ord)

-- | A pair of an action and a permission, such as @grant(foo)@.
data PermissionAction
  = Needs !PermissionName
  | Grants !PermissionName
  | Revokes !PermissionName
  | Denies !PermissionName
  | Waives !PermissionName
  deriving (Eq, Generic, Ord)

type PermissionActionSet = HashSet PermissionAction

data PermissionPresence
  = Has !PermissionName
  | Lacks !PermissionName
  deriving (Eq, Generic, Ord)

type PermissionPresenceSet = HashSet PermissionPresence

presencePermission :: PermissionPresence -> PermissionName
presencePermission = \ case
  Has p -> p
  Lacks p -> p

-- | Why a particular permission action is being applied.
data Reason
  = NoReason !NodeInfo
  | BecauseCall !Ident

reasonPos :: Reason -> NodeInfo
reasonPos (NoReason pos) = pos
reasonPos (BecauseCall (Ident _ _ pos)) = pos

type Enforcement = These FilePath FunctionName

data Restriction = !PermissionPresence `Implies` Expression

data Expression
  = Context !PermissionPresence
  | !Expression `And` !Expression
  | !Expression `Or` !Expression
  | Not !Expression
  deriving (Eq)

infixr 3 `And`
infixr 2 `Or`
infixr 1 `Implies`

instance IsString Expression where
  fromString = Context . Has . fromString

instance Show Restriction where
  show (p `Implies` e) = concat [show p, " -> ", show e]

instance Show Expression where
  showsPrec p = \ case
    Context presence -> shows presence
    a `And` b -> showParen (p > andPrec)
      $ showsPrec andPrec a . showString " & " . showsPrec andPrec b
    a `Or` b -> showParen (p > orPrec)
      $ showsPrec orPrec a . showString " & " . showsPrec orPrec b
    Not a -> showParen (p > notPrec)
      $ showString "!" . showsPrec notPrec a
    where
    andPrec = 3
    orPrec = 2
    notPrec = 10

instance Show PermissionName where
  show (PermissionName name) = Text.unpack name

instance Show PermissionAction where
  show = \ case
    Needs p -> concat ["needs(", show p, ")"]
    Grants p -> concat ["grants(", show p, ")"]
    Revokes p -> concat ["revokes(", show p, ")"]
    Denies p -> concat ["denies(", show p, ")"]
    Waives p -> concat ["waives(", show p, ")"]

instance Hashable PermissionAction

instance Show Reason where
  show = \ case
    NoReason _ -> "unspecified reason"
    BecauseCall (Ident name _ _) -> concat ["call to '", name, "'"]

instance Show PermissionPresence where
  show = \ case
    Has p -> concat ["has(", show p, ")"]
    Lacks p -> concat ["lacks(", show p, ")"]

instance Hashable PermissionPresence
