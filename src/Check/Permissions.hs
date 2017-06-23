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
      [ Restriction
        { restCondition = Has Strong name
        , restExpression = expr
        , restDescription = desc
        }
      | (name, decl) <- Map.toList $ configDeclarations config
      , (expr, desc) <- declRestrictions decl
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

    -- All functions are implicitly annotated with all permissions that are
    -- declared "implicit" in the config, unless waived by an annotation.
    implicitPermissions :: [PermissionName]
    implicitPermissions =
      [ name
      | (name, decl) <- Map.toList $ configDeclarations config
      , declImplicit decl
      ]

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

        -- For each permission action in function, plus implicit permissions not waived:
        permissionActions <- readIORef $ nodePermissions node
        let
          implicitPermissionActions =
            [ Needs p
            | p <- implicitPermissions
            , not $ Waives p `HashSet.member` permissionActions
            ]
        for_ (HashSet.toList permissionActions <> implicitPermissionActions) $ \ permissionAction -> do

          flip (IOVector.modify sites) 0 $ (<>) $ case permissionAction of
            -- If a function needs or revokes a permission, then its first call
            -- site must have that permission.
            Needs p -> site $ Has Weak p
            Revokes p -> site $ Has Weak p
            -- If a function grants or denies a permission, then its first call
            -- site must lack that permission.
            Grants p -> site $ Lacks p
            Denies p -> site $ Lacks p
            -- FIXME: Verify this.
            Waives{} -> mempty

        -- for each sequential statement in function:
        let
          callVertices = graphVertex <$> nodeCalls node

          processCallTree
            :: CallTree (Maybe Graph.Vertex)  -- input
            -> Int                            -- offset within current sequence
            -> IOVector Site                  -- current sequence
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

          processCallTree s@(Sequence a b) i v = do
            processCallTree a i v
            processCallTree b (i + callTreeBreadth a) v
            -- Assuming sequences are right-associative, if this is the root of
            -- a sequence:

            when (i == 0) $ do
              -- Propagate permissions backward through whole sequence.
              for_ (reverse [1 .. IOVector.length v - 2]) $ \ statement -> do
                after <- IOVector.read v statement
                flip (IOVector.modify v) (pred statement) $ \ before
                  -> before <> (foldr HashSet.delete after
                    $ concatMap (\p -> [Has Weak p, Lacks p])
                    $ map presencePermission
                    $ HashSet.toList before)

          processCallTree (Call (Just call)) i v = do
            let (Node { nodePermissions = callPermissionsRef }, callName, _) = graphLookup call
            callPermissions <- readIORef callPermissionsRef

            -- Propagate non-conflicting permissions forward.
            IOVector.write v (succ i)
              . HashSet.filter (not . conflicting)
              =<< IOVector.read v i

            -- Update permission presence (has/lacks/conflicts) according to
            -- permission actions (needs/denies/grants/revokes).
            for_ (HashSet.toList callPermissions) $ \ callPermission -> do
              case callPermission of

                -- If a call needs (resp. denies) a permission, its call site
                -- must have (lack) it. If the call site already lacks (has) it,
                -- we record the conflict.

                Needs p -> do
                  current <- IOVector.read v i
                  if Lacks p `HashSet.member` current
                    then IOVector.modify v ((<> site (Conflicts p)) . HashSet.delete (Lacks p)) i
                    else IOVector.modify v (<> site (Has Weak p)) i

                Denies p -> do
                  current <- IOVector.read v i
                  if Has Strong p `HashSet.member` current
                    then IOVector.modify v
                      ( (<> site (Conflicts p))
                      . HashSet.filter (\x -> not $ isHas x && presencePermission x == p)) i
                    else IOVector.modify v ((<> site (Lacks p))) i

                -- If a call grants (resp. revokes) a permission, its call site
                -- must lack (have) it, and the following call site must have
                -- (lack) it. If the current call site already has (lacks) it,
                -- we record the conflict. But if the following call site
                -- already lacks (has) it, we replace it to reflect the change
                -- in permission state.

                Grants p -> do
                  current <- IOVector.read v i
                  if Has Strong p `HashSet.member` current
                    then IOVector.modify v
                      ( (<> site (Conflicts p))
                      . HashSet.filter
                        (\ x -> not $ isHas x && presencePermission x == p)) i
                    else IOVector.modify v ((<> site (Lacks p))) i
                  IOVector.modify v ((<> site (Has Weak p)) . HashSet.delete (Lacks p)) $ succ i

                Revokes p -> do
                  current <- IOVector.read v i
                  if Lacks p `HashSet.member` current
                    then IOVector.modify v ((<> site (Conflicts p)) . HashSet.delete (Lacks p)) i
                    else IOVector.modify v ((<> site (Has Weak p))) i
                  IOVector.modify v
                    ( (<> site (Lacks p))
                    . HashSet.filter
                      (\ x -> not $ isHas x && presencePermission x == p)) $ succ i

                -- FIXME: Verify this.
                Waives{} -> pure ()

          -- Assume an unknown call has irrelevant permissions. I just know this
          -- is going to bite me later.
          processCallTree (Call Nothing) _ _ = pure ()
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
              when (Has Weak p `HashSet.member` initial && not (Lacks p `HashSet.member` initial)) $ do

                -- When the initial state has a permission, the function needs
                -- that permission.
                modifyIORef' permissionRef $ HashSet.insert $ Needs p

                -- When the initial state has a permission but the final state
                -- lacks it, the function revokes that permission.
                when (Lacks p `HashSet.member` final && not (Has Weak p `HashSet.member` final)) $ do
                  modifyIORef' permissionRef $ HashSet.insert $ Revokes p

              when (Lacks p `HashSet.member` initial && not (Has Weak p `HashSet.member` initial)) $ do

                -- When the initial state lacks a permission, the function
                -- denies that permission.
                -- modifyIORef' permissionRef $ HashSet.insert $ Denies p

                -- When the initial state lacks a permission but the final state
                -- has it, the function grants that permission.
                when (Has Weak p `HashSet.member` final && not (Lacks p `HashSet.member` final)) $ do
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
      sites <- liftIO $ Vector.freeze $ nodeSites node

       -- Report call sites with conflicting information.
      let conflicts = HashSet.filter conflicting $ mconcat $ Vector.toList sites
      unless (HashSet.null conflicts) $ do
        record True $ Error pos $ Text.concat $
          [ "conflicting information for permissions "
          , Text.pack $ show $ HashSet.toList conflicts
          , " in '"
          , name
          , "'"
          ]

      for_ (zip [0 :: Int ..] (Vector.toList sites)) $ \ (index, s) -> do
        let
          position = case index of
            0 -> ["before first call"]
            _ ->
              [ "at "
              , Text.pack $ show $ callTreeIndex (index - 1) $ nodeCalls node
              ]
        -- Report violated restrictions.
        for_ (restrictions) $ \ restriction -> do
          unless (evalRestriction s restriction) $ do
            record True $ Error pos $ Text.concat $
              [ "restriction "
              , Text.pack $ show restriction
              , " violated in '"
              , name
              {-
              , "' with permissions '"
              , Text.pack $ show $ HashSet.toList s
              -}
              , "' "
              ]
              <> position

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
evalRestriction context restriction
  | restCondition restriction `HashSet.member` context = go $ restExpression restriction
  | otherwise = True
  where
    go = \ case
      -- Since 'Conflicts' represents both 'Has' and 'Lacks', it matches both.
      Context p -> p `HashSet.member` context
        || Conflicts (presencePermission p) `HashSet.member` context
      a `And` b -> go a && go b
      a `Or` b -> go a || go b
      Not a -> not $ go a
       
conflicting :: PermissionPresence -> Bool
conflicting Conflicts{} = True
conflicting _ = False
