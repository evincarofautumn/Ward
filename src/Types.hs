{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Types where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef -- *
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Node (NodeInfo(..))
import Language.C.Data.Position (posFile, posRow)
import qualified Data.Text as Text

data Entry
  = Note !NodeInfo !Text
  | Warning !NodeInfo !Text
  | Error !NodeInfo !Text

instance Show Entry where
  show = \ case
    Note p t -> concat [posPrefix p, ": note: ", Text.unpack t]
    Warning p t -> concat [posPrefix p, ": warning: ", Text.unpack t]
    Error p t -> concat [posPrefix p, ": error: ", Text.unpack t]

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

newtype Logger a = Logger { runLogger :: IORef [Entry] -> IO a }

instance Functor Logger where
  fmap f (Logger g) = Logger (\ entries -> fmap f (g entries))

instance Applicative Logger where
  pure = Logger . const . return
  Logger f <*> Logger g = Logger (\ entries -> f entries <*> g entries)

instance Monad Logger where
  Logger f >>= g = Logger (\ entries -> flip runLogger entries . g =<< f entries)

instance MonadIO Logger where
  liftIO = Logger . const

record :: Entry -> Logger ()
record entry = Logger (\ entries -> modifyIORef' entries (entry :))

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

-- | An action to take on the context, given the permission from a
-- 'PermissionAction'.
data Action

  -- | The context must contain the given permission. This action does not
  -- change the context.
  = Need

  -- | After this action, the given permission is added to the context. The
  -- context may contain the permission already.
  | Grant

  -- | The context must contain the given permission. After this action, it will
  -- be removed from the context.
  | Revoke

  -- | The context may contain the given permission. During this function, it
  -- will be removed from the context. This can be used to waive permissions
  -- implicitly granted by "--grant".
  | Waive
  deriving (Eq, Ord)

newtype Permission = Permission Text
  deriving (Eq, IsString, Ord)

-- | A pair of an action and a permission, such as @grant(foo)@.
data PermissionAction = PermissionAction !Action !(Maybe Int) !Permission
  deriving (Eq, Ord)

-- | Why a particular permission action is being applied.
data Reason
  = NoReason !NodeInfo
  | BecauseCall !Ident

reasonPos :: Reason -> NodeInfo
reasonPos (NoReason pos) = pos
reasonPos (BecauseCall (Ident _ _ pos)) = pos

instance Show Permission where
  show (Permission name) = Text.unpack name

instance Show PermissionAction where
  show (PermissionAction action (Just subject) permission)
    = concat [show action, "(", show permission, "(", show subject, "))"]
  show (PermissionAction action Nothing permission)
    = concat [show action, "(", show permission, ")"]

instance Show Reason where
  show = \ case
    NoReason _ -> "unspecified reason"
    BecauseCall (Ident name _ _) -> concat ["call to '", name, "'"]

instance Show Action where
  show action = case action of
    Need -> "need"
    Grant -> "grant"
    Revoke -> "revoke"
    Waive -> "waive"
