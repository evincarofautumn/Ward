{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Check.Permissions
  ( Function(..)
  , process
  ) where

import Control.Monad (unless, when)
import Data.Foldable (forM_, toList)
import Data.Function (fix)
import Data.Graph (Graph, graphFromEdges)
import Data.IORef
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import Types
import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

type FunctionName = Text

data Node = Node

  -- | The permission actions of the function. This is set in the initial state
  -- by annotations, and updated as permissions are propagated.
  { nodePermissions :: !(IORef PermissionActionSet)
  , nodeAnnotations :: !(IORef PermissionActionSet)

  -- | The callees of this function.
  , nodeCalls :: !(CallTree FunctionName)

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
  , functionCalls :: !(CallTree FunctionName)
  }

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

    topologicallySorted = Graph.topSort calleeGraph
    sccs = Graph.dfs calleeGraph topologicallySorted

  -- for each scc in sccs:
  forM_ sccs $ \ scc -> do
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
        putStr $ strConcat [show $ nodeCalls node, "; "]
        let
          callVertices = vertexFromName <$> nodeCalls node
          vertexFromName n = fromMaybe
            (error $ strConcat
              [ "missing graph node for function '"
              , Text.unpack n
              , "'"
              ])
            $ graphVertex n

          process
            :: CallTree Graph.Vertex  -- input
            -> Int                    -- offset within current sequence
            -> [IOVector Site]        -- stack of choices
            -> IO (Site, Site)        -- call site permission presence sets before/after
          process _ _ [] = error "stack underflow in call site processing"
          process (Choice a b) i vs = do
            callsA <- IOVector.replicate (callTreeBreadth a + 1) mempty
            callsB <- IOVector.replicate (callTreeBreadth b + 1) mempty
            (beforeA, afterA) <- process a 0 (callsA : vs)
            (beforeB, afterB) <- process b 0 (callsB : vs)
            -- FIXME: mappend (union) might not be quite right here.
            pure (beforeA <> beforeB, afterA <> afterB)
          process (Sequence a b) i vs = do
            (beforeA, afterA) <- process a i vs
            (beforeB, afterB) <- process b (i + callTreeBreadth a) vs
            -- Not sure if afterA and beforeB need to be merged; they should already have been?
            pure (beforeA, afterB)
          process (Call call) i (v : vs) = do

            let (Node { nodePermissions = callPermissionsRef }, callName, _) = graphLookup call
            callPermissions <- readIORef callPermissionsRef

            putStr $ strConcat [show callName, " -> ", show callPermissions]

            -- Propagate permissions forward.
            IOVector.write v (succ i) =<< IOVector.read v i
            forM_ (HashSet.toList callPermissions) $ \ callPermission -> do
              case callPermission of

                -- If a call needs a permission, its call site must have it.
                Needs p -> do
                  IOVector.modify v (<> site (Has p)) i

                -- If a call grants a permission, its call site must lack it, and
                -- the following call site must have it.
                Grants p -> do
                  IOVector.modify v (<> site (Lacks p)) i
                  IOVector.modify v ((<> site (Has p)) . HashSet.delete (Lacks p)) $ succ i

                -- If a call revokes a permission, its call site must have it, and
                -- the following call site must lack it.
                Revokes p -> do
                  IOVector.modify v (<> site (Has p)) i
                  IOVector.modify v ((<> site (Lacks p)) . HashSet.delete (Has p)) $ succ i

            -- Return permissions before and after call.
            (,) <$> IOVector.read v i <*> IOVector.read v (succ i)

          process Nop _ _ = pure (mempty, mempty)

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

          putStr $ strConcat ["(relevant: ", show relevantPermissions, "); "]

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
      let (node, _name, _incoming) = graphLookup vertex

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
      forM_ (zip [0 :: Int ..] (Vector.toList sites)) $ \ (index, s) -> do
        -- Precondition: has(P) always implies !lacks(P) and vice versa.
        let implicitRestrictions = [Has p `Implies` Not (Context (Lacks p)) | Has p <- HashSet.toList s]
        forM_ (implicitRestrictions <> restrictions) $ \ restriction -> do
          unless (evalRestriction s restriction) $ do
            putStrLn $ strConcat $
              [ "restriction '"
              , show restriction
              , "' violated in '"
              , Text.unpack $ nodeName node
              , "' with permissions '"
              , show $ HashSet.toList s
              , "' "
              ]
              <> case index of
                0 -> ["before first call"]
                _ ->
                  [ "at call to "
                  , "TODO: function name"
                    {- intercalate "/" $ map (("'" <>) . (<> "'") . Text.unpack) $ Vector.toList
                      $ nodeCalls node Vector.! (index - 1) -}
                  ]

edgesFromFunctions :: [Function] -> IO [(Node, FunctionName, [FunctionName])]
edgesFromFunctions functions = do
  result <- newIORef []
  forM_ functions $ \ function -> do
    let name = functionName function
    permissions <- newIORef $ functionPermissions function
    annotations <- newIORef $ functionPermissions function
    sites <- IOVector.replicate (callTreeBreadth (functionCalls function) + 1) mempty
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
        , toList $ functionCalls function
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
       
