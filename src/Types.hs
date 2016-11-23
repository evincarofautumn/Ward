{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Types where

import Data.Text (Text)
import GHC.Exts (IsString(..))
import Language.C.Data.Ident (Ident(..))
import qualified Data.Text as Text

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
data PermissionAction = PermissionAction !Action !Permission
  deriving (Eq, Ord)

-- | Why a particular permission action is being applied.
data Reason
  = NoReason
  | BecauseCall !Ident

instance Show Permission where
  show (Permission name) = Text.unpack name

instance Show PermissionAction where
  show (PermissionAction action permission)
    = concat [show action, "(", show permission, ")"]

instance Show Reason where
  show = \ case
    NoReason -> "unspecified reason"
    BecauseCall (Ident name _ _) -> concat ["call to '", name, "'"]

instance Show Action where
  show action = case action of
    Need -> "need"
    Grant -> "grant"
    Revoke -> "revoke"
    Waive -> "waive"
