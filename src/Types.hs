{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Map as Map
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Permissions
--------------------------------------------------------------------------------

-- | A permission is a label on a function describing the operations it's
-- allowed to perform.
newtype PermissionName = PermissionName Text
  deriving (Eq, Hashable, IsString, Ord)

instance Show PermissionName where
  show (PermissionName name) = Text.unpack name

-- | A permission action is a pair of an action and a permission, such as
-- @grant(foo)@. An action describes the effect on the context before and after
-- a call to a function.
--
-- * @'Need' p@: The function indirectly needs @p@ because it calls other
--   functions that need or use @p@.
--
-- * @'Use' p@: The function directly needs @p@ because it makes use of the fact
--   that the permission is available. For example @use(locked)@ implies that
--   the function directly performs operations that require some lock held.
--
-- * @'Grant' p@: The function adds @p@ to the context; before the call, the
--   context 'Lacks' @p@, and afterward, it 'Has' @p@.
--
-- * @'Revoke' p@: The function removes @p@ from the context; before the call,
--   the context 'Has' @p@, and afterward, it 'Lacks' @p@.
--
-- * @'Deny' p@: The function cannot operate if @p@ is in the context. This
--   should never be necessary, but may be used to assert invariants and produce
--   better diagnostics.
--
-- * @'Waive' p@: If a permission @p@ was declared @implicit@ in the config
--   file, then it's implicitly granted to all functions unless they explicitly
--   'Waive' it.
--
data PermissionAction
  = Need !PermissionName
  | Use !PermissionName
  | Grant !PermissionName
  | Revoke !PermissionName
  | Deny !PermissionName
  | Waive !PermissionName
  deriving (Eq, Generic, Ord)


instance Show PermissionAction where
  show = \ case
    Need p -> concat ["need(", show p, ")"]
    Use p -> concat ["use(", show p, ")"]
    Grant p -> concat ["grant(", show p, ")"]
    Revoke p -> concat ["revoke(", show p, ")"]
    Deny p -> concat ["deny(", show p, ")"]
    Waive p -> concat ["waive(", show p, ")"]

instance Hashable PermissionAction

permissionActionName :: PermissionAction -> PermissionName
permissionActionName = \case
  Need n -> n
  Use n -> n
  Grant n -> n
  Revoke n -> n
  Deny n -> n
  Waive n -> n

-- | A set of permission actions. This is what a Ward annotation in a source
-- file represents.
--
type PermissionActionSet = HashSet PermissionAction

-- | Information about permissions before and after a given call site. These are
-- used during permission checking, inferred from 'PermissionAction's and the
-- 'CallTree' of each function.
--
-- * @'Has' p@: This call site has access to permission @p@. This appears when a
--   call @need@s @p@, before it @revoke@s @p@, or after it @grant@s @p@.
--
-- * @'Uses' p@: This call site makes use of permission @p@.
--
-- * @'Lacks' p@: This call site does not have access to permission @p@. This
--   appears after a call @revoke@s @p@, or before it @grant@s @p@.
--
-- * @'Conflicts' p@: This call site was inferred to have conflicting
--   information about @p@, that is, both 'Has' and 'Lacks' were inferred. All
--   'Conflicts' are reported as errors after checking.
--
data PermissionPresence
  = Has !PermissionName
  | Uses !PermissionName
  | Lacks !PermissionName
  | Conflicts !PermissionName
  deriving (Eq, Generic, Ord)


instance Show PermissionPresence where
  show = \ case
    Has p -> concat ["has(", show p, ")"]
    Uses p -> concat ["uses(", show p, ")"]
    Lacks p -> concat ["lacks(", show p, ")"]
    Conflicts p -> concat ["conflicts(", show p, ")"]

instance Hashable PermissionPresence

-- | A set of permission presences; each call site has one of these.
--
type PermissionPresenceSet = HashSet PermissionPresence

presencePermission :: PermissionPresence -> PermissionName
presencePermission = \ case
  Has p -> p
  Uses p -> p
  Lacks p -> p
  Conflicts p -> p

--------------------------------------------------------------------------------
-- Call graphs
--------------------------------------------------------------------------------

-- | Built from a collection of translation units, a 'NameMap' describes where a
-- function came from, its definition (if available), and the permission actions
-- described by its annotations.
--
type NameMap = Map Ident (NodeInfo, Maybe CFunDef, PermissionActionSet)

-- | Built from a 'NameMap', a 'CallMap' contains a compact 'CallTree' for each
-- function instead of the whole definition.
--
type CallMap = Map Ident (NodeInfo, CallTree Ident, PermissionActionSet)

-- | A 'CallTree' describes the calls that a function makes in its definition. A
-- 'Call' leaf node refers to a call site; a 'Nop' leaf refers to a statement
-- that makes no calls, such as @x = y;@. 'Nop' leaves are optimised out before
-- checking; see 'simplifyCallTree'.
--
-- 'Sequence' refers to functions that are called in order, such as:
--
-- > // Sequence (Call "foo") (Call "bar")
-- > foo ();
-- > bar ();
--
-- Sequences are handled one after the other during checking.
--
-- 'Choice' refers to functions that are called in different branches of an @if@
-- or @?:@, such as:
--
-- > // Sequence (Call "foo") (Choice (Call "bar") (Call "baz"))
-- > if (foo ()) {
-- >   bar ();
-- > } else {
-- >   baz ();
-- > }
--
-- Since we don't statically know which branch will be taken, choices are
-- handled during checking by checking each branch of the choice and then
-- merging their effects on the context.
--
data CallTree a
  = Sequence !(CallTree a) !(CallTree a)
  | Choice !(CallTree a) !(CallTree a)
  | Call !a
  | Nop
  deriving (Foldable, Functor, Traversable)

simplifyCallTree :: CallTree a -> CallTree a
simplifyCallTree (Sequence a b) = case (simplifyCallTree a, simplifyCallTree b) of
  (a', Nop) -> a'
  (Nop, b') -> b'
  (a', b') -> Sequence a' b'
simplifyCallTree (Choice a b) = case (simplifyCallTree a, simplifyCallTree b) of
  (a', Nop) -> a'
  (Nop, b') -> b'
  (a', b') -> Choice a' b'
simplifyCallTree leaf = leaf

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

callTreeIndex :: Int -> CallTree a -> CallTree a
callTreeIndex index tree = breadthFirst tree !! index
  where
    breadthFirst (Sequence a b) = breadthFirst a <> breadthFirst b
    breadthFirst t = [t]

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | An entry in the output, consisting of an error category, source location,
-- and error message. 'Error' is for fatal errors; 'Warning' for non-fatal
-- errors; and 'Note' for additional context or explanation.
data Entry
  = Note !NodeInfo !Text
  | Warning !NodeInfo !Text
  | Error !NodeInfo !Text
  deriving (Eq)

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

-- | A logger monad, providing access to a channel where entries can be sent
-- using 'record'. Sending @'Just' e@ for some entry @e@ should record the
-- entry, while sending @Nothing@ should close the channel.
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

-- | Sends an entry to the logging channel. The Boolean argument indicates
-- whether to send a message, which could be used to conveniently control
-- logging levels, like so:
--
-- > record (logLevel >= Debug) $ Note pos "Frobnicating..."
--
record :: Bool -> Entry -> Logger ()
record False _ = return ()
record True entry = Logger $ \ entries -> writeChan entries $ Just entry

-- | Closes the logging channel.
endLog :: Logger ()
endLog = Logger $ \ entries -> writeChan entries Nothing

-- | The Ward output mode: 'CompilerOutput' is for generating compiler-style
-- output, prefixing log entries with source location info:
--
-- > /path/to/file:line:column: message
--
-- 'HtmlOutput' is for generating an HTML report.
data OutputMode
  = CompilerOutput
  | HtmlOutput
  deriving (Eq)

-- | The Ward output action:
--
--   * 'AnalysisAction' runs the analyses with results formatted according to
--     the given 'OutputMode'.
data OutputAction
  = AnalysisAction !OutputMode
  deriving (Eq)

-- | Format an 'Entry' according to an 'OutputMode'.
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

-- | The header to output before entries for a given 'OutputMode'.
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

-- | The footer to output after entries for a given 'OutputMode'.
formatFooter :: OutputMode -> String -> String
formatFooter CompilerOutput extra = extra
formatFooter HtmlOutput extra = "\
  \</ul>\n\
  \\&" <> extra <> "\n\
  \</body>\n\
  \</html>\n\
  \\&"

-- | Partitions a list of entries into lists of notes, warnings, and errors.
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

-- | Why a particular permission action is being applied.
data Reason
  = NoReason !NodeInfo
  | BecauseCall !Ident


instance Show Reason where
  show = \ case
    NoReason _ -> "unspecified reason"
    BecauseCall (Ident name _ _) -> concat ["call to '", name, "'"]

reasonPos :: Reason -> NodeInfo
reasonPos (NoReason pos) = pos
reasonPos (BecauseCall (Ident _ _ pos)) = pos

type FunctionName = Text

--------------------------------------------------------------------------------
-- Configuration files
--------------------------------------------------------------------------------

-- | A 'Config' consists of a set of permission declarations and a set of
-- enforcements. This is parsed from a user-specified file; see @Config.hs@.
data Config = Config
  { configDeclarations :: !(Map PermissionName Declaration)
  , configEnforcements :: [Enforcement]
  } deriving (Eq, Show)

-- Multiple configs may be merged.
instance Monoid Config where
  mempty = Config mempty mempty
  mappend (Config declA enfA) (Config declB enfB) = Config
    (Map.unionWith (<>) declA declB)
    (enfA <> enfB)

-- | A 'Declaration' describes a permission that the user wants to check. It may
-- be @implicit@, in which case it is granted by default to every function that
-- does not explicitly 'Waive' it; it may have a human-readable 'Description' of
-- its purpose, such as @"permission to assume the foo lock is held"@; and it
-- may have additional 'Restriction's relating it to other permissions.
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

-- | A 'Restriction', declared in a config file, describes /relationships/
-- between permissions. For instance, a user might write a restriction like this
-- in their config:
--
-- > lock "permission to take the lock"
-- >   -> !locked "cannot take the lock recursively";
--
-- Then if the user attempts to use a function that would take the lock
-- recursively, Ward will report that this restriction has been violated.
--
-- If the /condition/ part of a restriction is present in the context, and the
-- /expression/ part evaluates to false in the context, then the violated
-- restriction is reported along with the human-readable /description/ if any.
--
data Restriction = Restriction
  { restCondition :: !PermissionPresence
  , restExpression :: !Expression
  , restDescription :: !(Maybe Description)
  }


instance Show Restriction where
  show r = case restDescription r of
    Just desc -> concat
      [ "\""
      , Text.unpack desc
      , "\" ("
      , implication
      , ")"
      ]
    Nothing -> implication
    where
      implication = concat
        [ show $ restCondition r
        , " -> "
        , show $ restExpression r
        ]

-- | An 'Expression' is an assertion about the presence of some permission in
-- the context ('Context') or a combination of these using Boolean operations
-- 'And', 'Or', and 'Not'.
data Expression
  = Context !PermissionPresence
  | !Expression `And` !Expression
  | !Expression `Or` !Expression
  | Not !Expression
  deriving (Eq)

-- These allow expressions to be used infix in Haskell code for readability.
infixr 3 `And`
infixr 2 `Or`

instance IsString Expression where
  fromString = Context . Has . fromString

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

-- | An 'Enforcement', declared in a config file, describes the functions that
-- Ward will force to be fully annotated. It may be:
--
-- * @'This' path@: enforce annotations for all functions declared or defined in
--   a path ending with @path@. (E.g., a public header.)
--
-- * @'That' name@: enforce annotations for all functions named @name@. (E.g., a
--   particular private function.)
--
-- * @'These' path name@ enforce annotations for a function named @name@ only if
--   declared or defined in the given @path@. (E.g., @static@ functions with
--   non-unique names.)
--
type Enforcement = These FilePath FunctionName

-- | A description is just an arbitrary docstring read from a config file.
type Description = Text
