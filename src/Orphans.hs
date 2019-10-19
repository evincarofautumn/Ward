{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Orphans() where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))

import Language.C.Data.Ident (Ident(..))
import Language.C.Data.Node (NodeInfo(..))
import Language.C.Data.Name (Name)
import Language.C.Data.Position

instance A.ToJSONKey Ident
instance A.FromJSONKey Ident
instance A.ToJSON Ident
instance A.FromJSON Ident
instance A.ToJSON NodeInfo
instance A.FromJSON NodeInfo
instance A.ToJSON Name
instance A.FromJSON Name
instance A.ToJSON Position where
  toJSON pos = A.object
    [ "offset"  .= posOffset pos
    , "file"    .= posFile pos
    , "row"     .= posRow pos
    , "col"     .= posColumn pos
    , "mparent" .= posParent pos
    ]
instance A.FromJSON Position where
  parseJSON = A.withObject "position" $ \o->
    position
      <$> o .: "offset"
      <*> o .: "file"
      <*> o .: "row"
      <*> o .: "col"
      <*> o .: "mparent"
