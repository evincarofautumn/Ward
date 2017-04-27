{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((***))
import Control.Monad (unless, when)
import Data.Foldable (forM_)
import Data.Function (fix)
import Data.Graph (Graph, graphFromEdges)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.IORef
import Data.List (foldl', intercalate, intersperse, nub, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Tree (Tree)
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import GHC.Generics (Generic)
import qualified Data.Array as Array
import qualified Data.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

type FunctionName = Text
type PermissionName = Text

data PermissionAction
  = Needs !PermissionName
  | Grants !PermissionName
  | Revokes !PermissionName
  deriving (Eq, Generic, Ord)

instance Show PermissionAction where
  show = \ case
    Needs p -> strConcat ["needs(", Text.unpack p, ")"]
    Grants p -> strConcat ["grants(", Text.unpack p, ")"]
    Revokes p -> strConcat ["revokes(", Text.unpack p, ")"]

instance Hashable PermissionAction

data PermissionPresence
  = Has !PermissionName
  | Lacks !PermissionName
  deriving (Eq, Generic, Ord)

presencePermission :: PermissionPresence -> PermissionName
presencePermission = \ case
  Has p -> p
  Lacks p -> p

instance Show PermissionPresence where
  show = \ case
    Has p -> strConcat ["has(", Text.unpack p, ")"]
    Lacks p -> strConcat ["lacks(", Text.unpack p, ")"]

instance Hashable PermissionPresence

type PermissionActionSet = HashSet PermissionAction
type PermissionPresenceSet = HashSet PermissionPresence

data Node = Node

  -- | The permission actions of the function. This is set in the initial state
  -- by annotations, and updated as permissions are propagated.
  { nodePermissions :: !(IORef PermissionActionSet)
  , nodeAnnotations :: !(IORef PermissionActionSet)

  -- | The callees of this function. The outer vector is sequential; the inner
  -- is parallel. For example:
  --
  -- > void foo(int x) {
  -- >   bar();
  -- >   if (x) { baz(); } else { quux(); }
  -- >   glurch();
  -- > }
  --
  -- > calls = [[bar], [baz, quux], [glurch]]
  --
  , nodeCalls :: !(Vector (Vector FunctionName))

  -- | One more than the number of callees, representing the permission state
  -- before and after each call.
  , nodeSites :: !(IOVector Site)
  , nodeName :: !FunctionName
  }

type Site = PermissionPresenceSet

site :: PermissionPresence -> Site
site = HashSet.singleton

data Function = Function
  { functionName :: !FunctionName
  , functionPermissions :: !PermissionActionSet
  , functionCalls :: !(Vector (Vector FunctionName))
  }

data Restriction = !PermissionPresence `Implies` Expression

data Expression
  = Context !PermissionPresence
  | !Expression `And` !Expression
  | !Expression `Or` !Expression
  | Not !Expression

infixr 3 `And`
infixr 2 `Or`
infixr 1 `Implies`

instance Show Restriction where
  show (p `Implies` e) = strConcat [show p, " -> ", show e]

instance Show Expression where
  showsPrec p = \ case
    Context p -> shows p
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

main :: IO ()
main = do
  process functions restrictions
  where
    functions :: [Function]
    functions =
      -- do() { begin(); end(); }
      [ Function "do" [Needs "lock"]
        [ ["begin"]
        , ["end"]
        ]
      -- begin() { lock(); go(); }
      , Function "begin" []
        [ ["lock"]
        , ["go"]
        ]
      -- end() { stop(); unlock(); }
      , Function "end" []
        [ ["stop"]
        , ["unlock"]
        ]
      -- go() {...}
      , Function "go" [Needs "locked"]
        [
        ]
      -- stop() {...}
      , Function "stop" [Needs "locked"]
        [
        ]
      -- lock() needs(lock), revokes(lock), grants(locked) {...}
      , Function "lock" [Needs "lock", Revokes "lock", Grants "locked"]
        [
        ]
      -- unlock() needs(locked), revokes(locked), grants(lock) {...}
      , Function "unlock" [Needs "locked", Revokes "locked", Grants "lock"]
        [
        ]
      ]

    restrictions :: [Restriction]
    restrictions =
      [ Has "locked" `Implies` Not (Context (Has "lock"))
      , Has "lock" `Implies` Not (Context (Has "locked"))
      ]

process :: [Function] -> [Restriction] -> IO ()
process functions restrictions = do
  edges <- edgesFromFunctions functions
  let
    callerGraph :: Graph
    graphLookup :: Graph.Vertex -> (Node, FunctionName, [FunctionName])
    graphVertex :: FunctionName -> Maybe Graph.Vertex
    (callerGraph, graphLookup, graphVertex) = graphFromEdges edges

    graphLookupName :: Graph.Vertex -> FunctionName
    graphLookupName = (\ (_, name, _) -> name) . graphLookup

    graphLookupNode :: Graph.Vertex -> Node
    graphLookupNode = (\ (node, _, _) -> node) . graphLookup

    calleeGraph :: Graph
    calleeGraph = Graph.transposeG callerGraph

    -- topologicallySorted = Graph.topSort calleeGraph
    sccs = Graph.scc calleeGraph

  -- for each scc in sccs:
  forM_ (reverse sccs) $ \ scc -> do
    -- while permissions are growing:
    growing <- newIORef True
    fix $ \ loop -> do
      writeIORef growing False

      -- for each function in scc:
      forM_ (Tree.flatten scc) $ \ vertex -> do
        let
          (node, name, _incoming) = graphLookup vertex
          sites = nodeSites node

        putStr $ Text.unpack name <> ": "

        -- for each permission action in function:
        permissionActions <- readIORef $ nodePermissions node
        forM_ (HashSet.toList permissionActions) $ \ permissionAction -> do

          flip (IOVector.modify sites) 0 $ (<>) . site $ case permissionAction of
            -- if a function needs or revokes a permission, then its first call
            -- site must have that permission.
            Needs p -> Has p
            Revokes p -> Has p
            -- if a function grants a permission, then its first call site must
            -- lack that permission.
            Grants p -> Lacks p

        -- for each sequential statement in function:
        let
          callVertices = fmap vertexFromName <$> nodeCalls node
          vertexFromName name = fromMaybe
            (error $ strConcat
              [ "missing graph node for function '"
              , Text.unpack name
              , "'"
              ])
            $ graphVertex name
        flip Vector.imapM_ callVertices $ \ statement calls -> do

          -- HACK: Should process all parallel calls.
          let call = calls Vector.! 0
          let (Node { nodePermissions = callPermissionsRef }, _, _) = graphLookup call
          callPermissions <- readIORef callPermissionsRef

          -- Propagate permissions forward.
          IOVector.write sites (succ statement) =<< IOVector.read sites statement
          forM_ (HashSet.toList callPermissions) $ \ callPermission -> do
            case callPermission of

              -- If a call needs a permission, its call site must have it.
              Needs p -> do
                IOVector.modify sites (<> site (Has p)) statement

              -- If a call grants a permission, its call site must lack it, and
              -- the following call site must have it.
              Grants p -> do
                IOVector.modify sites (<> site (Lacks p)) statement
                IOVector.modify sites ((<> site (Has p)) . HashSet.delete (Lacks p)) $ succ statement

              -- If a call revokes a permission, its call site must have it, and
              -- the following call site must lack it.
              Revokes p -> do
                IOVector.modify sites (<> site (Has p)) statement
                IOVector.modify sites ((<> site (Lacks p)) . HashSet.delete (Has p)) $ succ statement

        -- Propagate permissions backward.
        forM_ (reverse [1 .. IOVector.length sites - 2]) $ \ statement -> do
          after <- IOVector.read sites statement
          flip (IOVector.modify sites) (pred statement) $ \ before
            -> before <> (foldr HashSet.delete after
              $ concatMap (\p -> [Has p, Lacks p])
              $ map presencePermission
              $ HashSet.toList before)

        do
          currentSize <- HashSet.size <$> readIORef (nodePermissions node)

          -- For each "relevant" permission P in first & last callsites:
          initial <- IOVector.read sites 0
          final <- IOVector.read sites (IOVector.length sites - 1)
          let
            relevantPermissions = nub $ map presencePermission
              $ HashSet.toList initial <> HashSet.toList final

          forM_ relevantPermissions $ \ p -> do
            when (Has p `HashSet.member` initial) $ do

              -- When the initial state has a permission, the function needs
              -- that permission.
              modifyIORef' (nodePermissions node) $ HashSet.insert $ Needs p

              -- When the initial state has a permission but the final state
              -- lacks it, the function revokes that permission.
              when (Lacks p `HashSet.member` final) $ do
                modifyIORef' (nodePermissions node) $ HashSet.insert $ Revokes p

            when (Lacks p `HashSet.member` initial && Has p `HashSet.member` final) $ do
              -- When the initial state lacks a permission but the final state
              -- has it, the function grants that permission.
              modifyIORef' (nodePermissions node) $ HashSet.insert $ Grants p

          modifiedSize <- HashSet.size <$> readIORef (nodePermissions node)

          -- If we added permissions, the inferred set of permissions for this
          -- SCC may still be growing, so we re-process the SCC until we reach a
          -- fixed point.
          --
          -- TODO: Limit the number of iterations to prevent infinite loops.
          writeIORef growing $ modifiedSize > currentSize

        print =<< readIORef (nodePermissions node)

      do
        shouldContinue <- readIORef growing
        if shouldContinue then loop else pure ()

  -- Check consistency.
  forM_ sccs $ \ scc -> do
    forM_ (Tree.flatten scc) $ \ vertex -> do
      let (node, name, _incoming) = graphLookup vertex

      annotations <- readIORef $ nodeAnnotations node
      unless (HashSet.null annotations) $ do
        permissions <- readIORef $ nodePermissions node
        let difference = HashSet.difference permissions annotations
        -- Annotations, if present, must mention all inferred permissions.
        unless (HashSet.null difference) $ do
          putStrLn $ strConcat
            [ "missing annotations in '"
            , Text.unpack $ nodeName node
            , "': "
            , show $ HashSet.toList difference
            ]
        -- The inferred type must be consistent.
        forM_ (HashSet.toList permissions) $ \ permission -> do
          let
            mInconsistency = case permission of
              Needs p
                | Grants p `HashSet.member` permissions
                -> Just (Grants p)
              Grants p
                | Revokes p `HashSet.member` permissions
                -> Just (Revokes p)
              _ -> Nothing
          flip (maybe (pure ())) mInconsistency $ \ inconsistency -> do
            putStrLn $ strConcat
              [ "inferred inconsistent permission actions in '"
              , Text.unpack $ nodeName node
              , "': "
              , show permission
              , " is incompatible with "
              , show inconsistency
              ]

      -- There should be no contradictory information at call sites.
      -- TODO: Generalize to any restriction.
      sites <- Vector.freeze $ nodeSites node
      forM_ (zip [0..] (Vector.toList sites)) $ \ (index, site) -> do
        -- Precondition: has(P) always implies !lacks(P) and vice versa.
        let implicitRestrictions = [Has p `Implies` Not (Context (Lacks p)) | Has p <- HashSet.toList site]
        forM_ (implicitRestrictions <> restrictions) $ \ restriction -> do
          unless (evalRestriction site restriction) $ do
            putStrLn $ strConcat $
              [ "restriction '"
              , show restriction
              , "' violated in '"
              , Text.unpack $ nodeName node
              , "' with permissions '"
              , show $ HashSet.toList site
              , "' "
              ]
              <> case index of
                0 -> ["before first call"]
                _ ->
                  [ "at call to "
                  , intercalate "/" $ map (("'" <>) . (<> "'") . Text.unpack) $ Vector.toList
                    $ nodeCalls node Vector.! (index - 1)
                  ]

edgesFromFunctions :: [Function] -> IO [(Node, FunctionName, [FunctionName])]
edgesFromFunctions functions = do
  result <- newIORef []
  forM_ functions $ \ function -> do
    let name = functionName function
    permissions <- newIORef $ functionPermissions function
    annotations <- newIORef $ functionPermissions function
    sites <- IOVector.replicate (Vector.length (functionCalls function) + 1) mempty
    let
      node =
        ( Node
          { nodePermissions = permissions
          , nodeAnnotations = annotations
          , nodeCalls = functionCalls function
          , nodeSites = sites
          , nodeName = name
          }
        , name
        , Vector.toList $ Vector.concat $ Vector.toList $ functionCalls function
        )
    modifyIORef' result (node :)
  readIORef result

strConcat :: [String] -> String
strConcat = concat

evalRestriction :: PermissionPresenceSet -> Restriction -> Bool
evalRestriction context (p `Implies` e)
  | p `HashSet.member` context = go e
  | otherwise = True
  where
    go = \ case
      Context p' -> p' `HashSet.member` context
      a `And` b -> go a && go b
      a `Or` b -> go a || go b
      Not a -> not $ go a
