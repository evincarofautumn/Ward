{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Check.Permissions
  ( Function(..)
  , process
  ) where

import Config
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, toList)
import Data.Function (fix)
import Data.Graph (Graph, graphFromEdges)
import Data.IORef
import Data.List (isSuffixOf, nub)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.These
import Data.Vector.Mutable (IOVector)
import Language.C.Data.Node (NodeInfo, posOfNode)
import Language.C.Data.Position (posFile)
import Types
import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

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
  , nodePos :: !NodeInfo
  }

type Site = PermissionPresenceSet

site :: PermissionPresence -> Site
site = HashSet.singleton

data Function = Function
  { functionPos :: !NodeInfo
  , functionName :: !FunctionName
  , functionPermissions :: !PermissionActionSet
  , functionCalls :: !(CallTree FunctionName)
  }

process :: [Function] -> Config -> Logger ()
process functions config = do

  -- Get permissions from functions that require annotations.
  let
    requiresAnnotation :: FunctionName -> NodeInfo -> Bool
    requiresAnnotation name info = or
      [ case enforcement of
        This path' -> path' `isSuffixOf` path
        That name' -> name == name'
        These path' name' -> path' `isSuffixOf` path && name == name'
      | enforcement <- configEnforcements config
      ]
      where
        path = posFile $ posOfNode info

    requiredAnnotations :: [(FunctionName, PermissionActionSet)]
    requiredAnnotations = 
      [ (name, permissions)
      | Function
        { functionName = name
        , functionPos = info
        , functionPermissions = permissions
        } <- functions
      , requiresAnnotation name info
      ]

    -- TODO: Keep full declarations for error reporting.
    restrictions =
      [ Has name `Implies` expr
      | (name, decl) <- Map.toList $ configDeclarations config
      , (expr, _desc) <- declRestrictions decl
      ]

  -- Build call graph.
  edges <- liftIO $ edgesFromFunctions functions
  let
    callerGraph :: Graph
    graphLookup :: Graph.Vertex -> (Node, FunctionName, [FunctionName])
    graphVertex :: FunctionName -> Maybe Graph.Vertex
    (callerGraph, graphLookup, graphVertex) = graphFromEdges edges

    calleeGraph :: Graph
    calleeGraph = Graph.transposeG callerGraph

    topologicallySorted = Graph.topSort calleeGraph
    sccs = Graph.dfs calleeGraph topologicallySorted

  -- Propagate permission information through the graph.
  liftIO $ for_ sccs $ \ scc -> do
    -- while permissions are growing:
    growing <- newIORef True
    fix $ \ loop -> do
      writeIORef growing False

      -- for each function in scc:
      for_ (Tree.flatten scc) $ \ vertex -> do
        let
          (node, name, _incoming) = graphLookup vertex
          sites = nodeSites node

        -- for each permission action in function:
        permissionActions <- readIORef $ nodePermissions node
        for_ (HashSet.toList permissionActions) $ \ permissionAction -> do

          flip (IOVector.modify sites) 0 $ (<>) $ case permissionAction of
            -- If a function needs or revokes a permission, then its first call
            -- site must have that permission.
            Needs p -> site $ Has p
            Revokes p -> site $ Has p
            -- If a function grants or denies a permission, then its first call
            -- site must lack that permission.
            Grants p -> site $ Lacks p
            Denies p -> site $ Lacks p
            -- FIXME: Verify this.
            Waives p -> mempty

        -- for each sequential statement in function:
        let
          callVertices = vertexFromName <$> nodeCalls node
          vertexFromName n = fromMaybe
            (error $ strConcat
              [ "missing graph node for function '"
              , Text.unpack n
              , "'"
              ])
            $ graphVertex n

          processCallTree
            :: CallTree Graph.Vertex  -- input
            -> Int                    -- offset within current sequence
            -> IOVector Site          -- current sequence
            -> IO ()

          processCallTree (Choice a b) i v = do
            callsA <- IOVector.replicate (callTreeBreadth a + 1) mempty
            callsB <- IOVector.replicate (callTreeBreadth b + 1) mempty
            processCallTree a 0 callsA
            processCallTree b 0 callsB
            beforeA <- IOVector.read callsA 0
            afterA <- IOVector.read callsA (IOVector.length callsA - 1)
            beforeB <- IOVector.read callsB 0
            afterB <- IOVector.read callsB (IOVector.length callsB - 1)
            IOVector.write v i (beforeA <> beforeB)
            IOVector.write v (succ i) (afterA <> afterB)

          processCallTree (Sequence a b) i v = do
            processCallTree a 0 v
            processCallTree b (callTreeBreadth a) v
            -- Assuming sequences are right-associative, if this is the root of
            -- a sequence:
            when (i == 0) $ do
              -- Propagate permissions backward through whole sequence.
              for_ (reverse [1 .. IOVector.length v - 2]) $ \ statement -> do
                after <- IOVector.read v statement
                flip (IOVector.modify v) (pred statement) $ \ before
                  -> before <> (foldr HashSet.delete after
                    $ concatMap (\p -> [Has p, Lacks p])
                    $ map presencePermission
                    $ HashSet.toList before)

          processCallTree (Call call) i v = do
            let (Node { nodePermissions = callPermissionsRef }, callName, _) = graphLookup call
            callPermissions <- readIORef callPermissionsRef

            -- Propagate permissions forward.
            IOVector.write v (succ i) =<< IOVector.read v i
            for_ (HashSet.toList callPermissions) $ \ callPermission -> do
              case callPermission of

                -- If a call needs a permission, its call site must have it.
                Needs p -> do
                  IOVector.modify v (<> site (Has p)) i

                -- If a call denies a permission, its call site must lack it.
                Denies p -> do
                  IOVector.modify v (<> site (Lacks p)) i

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

                -- FIXME: Verify this.
                Waives p -> pure ()

          processCallTree Nop _ _ = pure ()

          permissionsFromCallSites :: IORef PermissionActionSet -> IOVector Site -> IO Bool
          permissionsFromCallSites permissionRef callsites = do
            currentSize <- HashSet.size <$> readIORef permissionRef

            -- For each "relevant" permission P in first & last callsites:
            initial <- IOVector.read callsites 0
            final <- IOVector.read callsites (IOVector.length callsites - 1)
            let
              relevantPermissions = nub $ map presencePermission
                $ HashSet.toList initial <> HashSet.toList final

            -- The seemingly redundant side conditions here prevent spurious
            -- error messages from inconsistent permissions.

            for_ relevantPermissions $ \ p -> do
              when (Has p `HashSet.member` initial && not (Lacks p `HashSet.member` initial)) $ do

                -- When the initial state has a permission, the function needs
                -- that permission.
                modifyIORef' permissionRef $ HashSet.insert $ Needs p

                -- When the initial state has a permission but the final state
                -- lacks it, the function revokes that permission.
                when (Lacks p `HashSet.member` final && not (Has p `HashSet.member` final)) $ do
                  modifyIORef' permissionRef $ HashSet.insert $ Revokes p

              when (Lacks p `HashSet.member` initial && not (Has p `HashSet.member` initial)) $ do

                -- When the initial state lacks a permission, the function
                -- denies that permission.
                modifyIORef' permissionRef $ HashSet.insert $ Needs p

                -- When the initial state lacks a permission but the final state
                -- has it, the function grants that permission.
                when (Has p `HashSet.member` final && not (Lacks p `HashSet.member` final)) $ do
                  modifyIORef' permissionRef $ HashSet.insert $ Grants p

            modifiedSize <- HashSet.size <$> readIORef permissionRef

            -- If we added permissions, the inferred set of permissions for this
            -- SCC may still be growing, so we re-process the SCC until we reach a
            -- fixed point.
            --
            -- TODO: Limit the number of iterations to prevent infinite loops.
            pure $ modifiedSize > currentSize

        processCallTree callVertices 0 sites
        writeIORef growing =<< permissionsFromCallSites (nodePermissions node) sites

      do
        shouldContinue <- readIORef growing
        if shouldContinue then loop else pure ()

  -- Check consistency.
  for_ sccs $ \ scc -> do
    for_ (Tree.flatten scc) $ \ vertex -> do
      let
        (node, _name, _incoming) = graphLookup vertex
        name = nodeName node
        pos = nodePos node
      annotations <- liftIO $ readIORef $ nodeAnnotations node
      permissions <- liftIO $ readIORef $ nodePermissions node

      -- If a function has required annotations, ensure the annotation
      -- mentions all inferred permissions.
      for_ (lookup name requiredAnnotations) $ \ userAnnotated -> do
        -- I think this should generally be equal to 'inferredNotDeclared'.
        let implicit = HashSet.difference permissions userAnnotated
        unless (HashSet.null implicit) $ do
          record True $ Error pos $ Text.concat
            [ "missing required annotation on '"
            , name
            , "'; annotation "
            , Text.pack $ show $ HashSet.toList userAnnotated
            , " is missing: "
            , Text.pack $ show $ HashSet.toList implicit
            ]

      unless (HashSet.null annotations) $ do
        let inferredNotDeclared = HashSet.difference permissions annotations

        -- Annotations, if present, must mention all inferred permissions.
        unless (HashSet.null inferredNotDeclared) $ do
          record True $ Error pos $ Text.concat
            [ "annotation on '"
            , name
            , "' is missing these permissions: "
            , Text.pack $ show $ HashSet.toList inferredNotDeclared
            ]
        -- The inferred type must be consistent.
        for_ (HashSet.toList permissions) $ \ permission -> do
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
            record True $ Error pos $ Text.concat
              [ "inferred inconsistent permissions for '"
              , name
              , "': "
              , Text.pack $ show permission
              , " is incompatible with "
              , Text.pack $ show inconsistency
              ]

      -- There should be no contradictory information at call sites.
      -- TODO: Generalize to any restriction.
      sites <- liftIO $ Vector.freeze $ nodeSites node
      for_ (zip [0 :: Int ..] (Vector.toList sites)) $ \ (index, s) -> do
        -- Precondition: has(P) always implies !lacks(P) and vice versa.
        let implicitRestrictions = [Has p `Implies` Not (Context (Lacks p)) | Has p <- HashSet.toList s]
        for_ (implicitRestrictions <> restrictions) $ \ restriction -> do
          unless (evalRestriction s restriction) $ do
            record True $ Error pos $ Text.concat $
              [ "restriction '"
              , Text.pack $ show restriction
              , "' violated in '"
              , name
              , "' with permissions '"
              , Text.pack $ show $ HashSet.toList s
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
  for_ functions $ \ function -> do
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
          , nodePos = functionPos function
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
       