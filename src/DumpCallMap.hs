{-# language ScopedTypeVariables #-}
module DumpCallMap (encodeCallMap, hPutCallMap) where

import Types (CallMap
             , CallTree(..)
             , PermissionAction(..)
             , PermissionActionSet
             , PermissionName(..)
             , permissionActionName
             )
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Builder as B
import Data.Foldable
import qualified Data.Map as Map
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text.Encoding as DTE
import System.IO (Handle, hFlush)

import Language.C.Data.Ident (Ident, identToString)
import Language.C.Data.Node (NodeInfo, CNode(nodeInfo))
import Language.C.Data.Name (Name)
import qualified Language.C.Data.Node as Node
import qualified Language.C.Data.Position as Pos

type CallMapItem = (NodeInfo, CallTree Ident, PermissionActionSet)

-- | Encode the given callmap as a "Data.ByteString
encodeCallMap :: CallMap -> B.Builder
encodeCallMap = form "callmap" True . Map.foldMapWithKey encodeCallMapItem

-- | @hPutCallMap handle callmap@ writes @callmap@ to the I/O handle @handle@.
-- The representation preserves all the source attributes of the underlying C file.
-- Identifiers are stored in UTF-8.
hPutCallMap :: MonadIO m => Handle -> CallMap -> m ()
hPutCallMap handle callMap = liftIO $ do
  B.hPutBuilder handle $ encodeCallMap callMap
  hFlush handle

-- | @emit s nl b@ emits @"(s\nb)"@ if @nl@ is 'True' or @"(s b)"@ if @nl@ is 'False.
form :: String -> Bool -> B.Builder -> B.Builder
form s nl b = parens (B.stringUtf8 s <> (if nl then newline else space) <> b)

newline, space, dblQuote, openParen, closeParen :: B.Builder
newline = B.charUtf8 '\n'
space = B.charUtf8 ' '
dblQuote = B.charUtf8 '"'
openParen = (B.charUtf8 '(')
closeParen = (B.charUtf8 ')')

parens :: B.Builder -> B.Builder
parens = enclose openParen closeParen

dblQuotes :: B.Builder -> B.Builder
dblQuotes = enclose dblQuote dblQuote

enclose :: B.Builder -> B.Builder -> B.Builder -> B.Builder
enclose open close b = open <> b <> close

encodeCallMapItem :: Ident -> CallMapItem -> B.Builder
encodeCallMapItem f (ni, tree, ps) =
  let
    body =
      [ encodeIdent f, space, encodeNodeInfo ni , newline
      , encodePerms ps, newline
      , form "calltree" True (encodeCallTree encodeIdent tree)
      ]
  in form "function" False (mconcat body) <> newline

-- Our static function idents look like @path`name@ where @path@ is a filepath
-- (with spaces, etc), so we need to quote the name.
encodeIdent :: Ident -> B.Builder
encodeIdent ident = form "ident" False (encodeIdentName ident <> space <> encodeNodeInfo (nodeInfo ident))
  where
    encodeIdentName = dblQuotes . B.stringUtf8 . identToString

encodeNodeInfo :: NodeInfo -> B.Builder
encodeNodeInfo ni =
  if Node.isUndefNode ni then B.stringUtf8 "noNode"
  else let
    position = encodePositionSpan (Node.posOfNode ni) (Node.getLastTokenPos ni)
    in case Node.nameOfNode ni of
         Just name -> form "node" False (encodeName name <> position)
         Nothing -> form "onlypos" False position

encodeName :: Name -> B.Builder
encodeName = B.intDec . fromEnum

encodePositionSpan :: Pos.Position -> Pos.PosLength -> B.Builder
encodePositionSpan pstart pl
  | Pos.isNoPos pstart = B.stringUtf8 "noPos"
  | Pos.isBuiltinPos pstart = B.stringUtf8 "builtin"
  | Pos.isInternalPos pstart = B.stringUtf8 "internal"
  | otherwise =
      let
        body =
          [ encodeSourcePos pstart
          , B.intDec (snd pl)
          , encodeSourcePos (fst pl)
          ]
      in form "span" False $ mconcat $ intersperse space body

-- N.B. Assumes Pos.isSourcePos would return True
--
-- TODO: Since language-c 0.7 we can now also get the include stack via
-- 'Pos.posParent' which may be useful for error messages for static inline
-- functions.  For now we don't both emiting it (or consuming it in
-- "ParseCallMap").
encodeSourcePos :: Pos.Position -> B.Builder
encodeSourcePos p =
  let
    body =
      [ dblQuotes (B.stringUtf8 $ Pos.posFile p)
      , B.intDec (Pos.posOffset p)
      , B.intDec (Pos.posRow p)
      , B.intDec (Pos.posColumn p)
      ]
  in form "source" False $ mconcat $ intersperse space body

encodeCallTree :: forall a . (a -> B.Builder) -> CallTree a -> B.Builder
encodeCallTree enc = goSeq
  where
    -- as long as t is a Nop or Sequence output as is, otherwise output a new
    -- form.
    goSeq :: CallTree a -> B.Builder
    goSeq t = case t of
      Nop -> mempty
      Sequence t1 t2 -> go t1 <> newline <> goSeq t2
      _ -> go t
    go :: CallTree a -> B.Builder
    go t = case t of
      Choice t1 t2 -> form "choice" True (go t1 <> newline <> go t2)
      Call c -> form "call" False (enc c)
      _ -> form "seq" True (goSeq t)

encodePerms :: PermissionActionSet -> B.Builder
encodePerms =
  form "actions" False
  . mconcat . intersperse space
  . map encodePermissionAction . toList

encodePermissionAction :: PermissionAction -> B.Builder
encodePermissionAction pa =
  permissionForm (encodePermissionName $ permissionActionName pa)
  where
    permissionForm = form ident False
    ident = case pa of
      Need {} -> "need"
      Grant {} -> "grant"
      Revoke {} -> "revoke"
      Deny {} -> "deny"
      Waive {} -> "waive"
      Use {} -> "use"

encodePermissionName :: PermissionName -> B.Builder
encodePermissionName (PermissionName t) = DTE.encodeUtf8Builder t
