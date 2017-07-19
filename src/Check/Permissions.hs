{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Check.Permissions
  ( Function(..)
  , process
  , validatePermissions
  ) where

import Algebra.Algebra
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, toList)
import Data.Function (fix)
import Data.Graph (Graph, graphFromEdges)
import Data.IORef
import Data.List (foldl', isSuffixOf)
import Data.Monoid ((<>), Any(..))
import Data.Vector.Mutable (IOVector)
import Language.C.Data.Ident (Ident)
import Language.C.Data.Node (NodeInfo, posOfNode)
import Language.C.Data.Position (posFile)
import Types
import qualified Data.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as IOVector

-- | A function given as input to the permission checking algorithm.
data Function = Function
  {
  -- | The source location where the function was declared, or where it was
  -- defined if there was no declaration.
    functionPos :: !NodeInfo

  -- | The name of the function, prefixed with its file path if @static@.
  , functionName :: !FunctionName

  -- | The permission actions declared for this function in the source file.
  , functionPermissions :: !PermissionActionSet

  -- | A tree of callees of this function.
  , functionCalls :: !(CallSequence FunctionName)
  }
  deriving (Show)

-- | A node in the call graph, representing a function and information about
-- permissions at each of its call sites.
data Node = Node
  {
  -- | The permission actions of the function. This is set in the initial state
  -- by annotations, and updated as permissions are propagated.
    nodePermissions :: !(IORef PermissionActionSet)

  -- | The annotated permission actions of the function.
  , nodeAnnotations :: !(IORef PermissionActionSet)

  -- | The callees of this function.
  , nodeCalls :: !(CallSequence FunctionName)

  -- | One more than the number of callees, representing the permission state
  -- before and after each call.
  , nodeSites :: !(IOVector Site)

  -- | The original name of this function, for error reporting.
  , nodeName :: !FunctionName

  -- | The original source location of this function, for error reporting.
  , nodePos :: !NodeInfo
  }

-- | A call 'Site' is represented by a set of 'PermissionPresence's, describing
-- which permissions are available ('Has'), needed ('Uses'), unavailable
-- ('Lacks'), or have conflicting information ('Conflicts') at each call site
-- within each function.
type Site = PermissionPresenceSet

-- | 'process' infers permissions for a call graph specified by a list of
-- 'Function's and verifies their consistency and correctness according to a
-- 'Config'.
--
-- The permission checking algorithm is fairly straightforward. We are given a
-- set of functions; each function has some associated permission actions and a
-- tree of calls to other functions.
--
-- > // Input
-- >
-- > void begin  () __attribute__ ((ward (grant  (perm))));
-- > void end    () __attribute__ ((ward (revoke (perm))));
-- > void truthy () __attribute__ ((ward (need   (perm))));
-- > void falsy  () __attribute__ ((ward (need   (perm))));
-- >
-- > void outer () {
-- >   if (begin ()) {
-- >     truthy ();
-- >   } else {
-- >     falsy ();
-- >   }
-- >   end ();
-- > }
--
-- > // Call tree
-- > begin = end = truthy = falsy = nop
-- > outer = begin & (truthy | falsy) & end
--
-- We take the top-level 'Sequence' of a call tree and flatten it into a vector
-- of call 'Site' info, initially empty. Each cell represents the state /before/
-- and /after/ a call, so there is one more 'Site' than calls in the tree.
--
-- > outer = [ {}  -- state before 'begin'
-- >               -- call to 'begin'
-- >         , {}  -- state after 'begin', before 'if'
-- >               -- 'if'
-- >         , {}  -- state after 'if', before 'end'
-- >               -- call to 'end'
-- >         , {}  -- state after 'end'
-- >         ]
--
-- For each call in the tree, we add information to the 'Site' before and after
-- the call according to the callee's permission actions. After one step the
-- state looks like this:
--
-- > outer = [ {lacks(perm)}  -- 'perm' can't already be in the context because...
-- >                          -- ...'begin' grants 'perm'...
-- >         , {has(perm)}    -- ...after which 'perm' is in the context.
-- >         , {}
-- >         , {}
-- >         ]
--
-- When we reach a 'Choice', we create a new sub-vector for each branch of the
-- 'Choice' and check its 'Sequence' of calls recursively in the same way as
-- the top-level sequence.
--
-- Creating the vectors:
--
-- > choice-A = [ {}  -- state before 'truthy'
-- >                  -- call to 'truthy'
-- >            , {}  -- state after 'truthy'
-- >            ]
-- >
-- > choice-B = [ {}  -- state before 'falsy'
-- >                  -- call to 'falsy'
-- >            , {}  -- state after 'falsy'
-- >            ]
--
-- Filling them in:
--
-- > choice-A = [ {has(perm)}  -- 'perm' must be in the context because...
-- >                           -- ...'truthy' needs 'perm'...
-- >            , {has(perm)}  -- ...and doesn't change the context.
-- >            ]
-- >
-- > choice-B = [ {has(perm)}  -- 'perm' must be in the context because...
-- >                           -- ...'falsy' needs 'perm'...
-- >            , {has(perm)}  -- ...and doesn't change the context.
-- >            ]
--
-- We then merge the effects of the branches of the choice, and treat it as a
-- single call, discarding the sub-vectors.
--
-- > outer = [ {lacks(perm)}
-- >         , {has(perm)}   -- 'perm' was already in the context...
-- >         , {has(perm)}   -- ...and the 'if' doesn't change that.
-- >         , {}
-- >         ]
--
-- After that, we can return up a level, and continue processing the rest of the
-- sequence we came from.
--
-- > outer = [ {lacks(perm)}
-- >         , {has(perm)}
-- >         , {has(perm)}    -- 'perm' must already be in the context because...
-- >                          -- ...'end' revokes 'perm'...
-- >         , {lacks(perm)}  -- ...after which 'perm' is not in the context.
-- >         ]
--
-- (This omits the details of how permissions are /propagated/ through a
-- function and between functions, which are explained inline.)
--
-- From the initial and final call sites of this sequence, we can deduce the
-- permission actions of the whole function. This is a trivial example: the net
-- effect of the function is @{lacks(perm)} -> {lacks(perm)}@, from which we
-- deduce no permission actions; @outer@ uses @perm@ entirely locally, so it
-- requires no annotations.
--
-- Things become more interesting when accounting for more complex uses of
-- permissions, as well as permission errors. This is the whole point of Ward:
-- to report inconsistencies and violations of assertions to help catch bugs.
--
-- After permission information has been inferred for all call sites in the call
-- graph, we check the result for consistency and report errors.
--
-- The first and most basic form of error is a /conflict/: when we infer that a
-- call site both @has@ and @lacks@ a permission, we know that someone must be
-- using permissions incorrectly. For example, if we called @begin@ twice in the
-- example above, we would have this call tree:
--
-- > outer = begin & begin & ...
--
-- We would start by inferring @{lacks(perm)} -> {has(perm)}@ for the first call
-- to @begin@:
--
-- > outer = [ {lacks(perm)}
-- >         , {has(perm)}
-- >         , {}
-- >         ...
-- >         ]
--
-- But for the second call, we would also infer @{lacks(perm)} -> {has(perm)}@:
--
-- > outer = [ {lacks(perm)}
-- >         , {has(perm),lacks(perm)}  -- Conflict!
-- >         , {has(perm)}
-- >         ...
-- >         ]
--
-- Whenever we would infer @{has(p),lacks(p)}@ for some permission @p@, we
-- replace it with @{conflicts(p)}@ to record the conflict. This ensures that we
-- don't continue to propagate any inconsistent permission information, so we
-- can avoid reporting many redundant errors.
--
-- The other forms of errors come from /restrictions/ and /enforcements/.
--
-- Restrictions describe relationships between permissions. If a call site
-- /uses/ a permission that has a restriction, then we evaluate the
-- corresponding expression on the context and report an error if it's false.
--
-- Enforcements describe which functions must be annotated. If a function
-- matches an enforcement (by path or name), then we report any permission
-- actions that were inferred but not specified in an annotation.
--
process :: [Function] -> Config -> Logger ()
process functions config = do

  -- We find all functions that require annotations according to enforcements in
  -- the config, and collect their annotations for checking later.
  let
    requiresAnnotation :: FunctionName -> NodeInfo -> Bool
    requiresAnnotation name info = or
      [ case enforcement of
        EnforcePath path' -> path' `isSuffixOf` path
        EnforceFunction name' -> name == name'
        EnforcePathFunction path' name' -> path' `isSuffixOf` path && name == name'
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

    restrictions =
      [ Restriction
        { restName = name
        , restExpression = expr
        , restDescription = desc
        }
      | (name, decl) <- Map.toList $ configDeclarations config
      , (expr, desc) <- declRestrictions decl
      ]

  -- Next, we build the call graph, transpose it to obtain a callee graph, and
  -- partition it into strongly connected components (SCCs), which we check in
  -- dependency order. This is how we propagate permission information between
  -- functions.

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

  -- Then we propagate permission information through the graph.

  -- For each SCC:
  liftIO $ for_ sccs $ inferPermissionsSCC implicitPermissions graphLookup graphVertex

  -- Finally, we check the whole call graph, reporting conflicting information
  -- at call sites, missing annotations (from enforcements), and violated
  -- restrictions.

  for_ sccs $ reportSCC
    implicitPermissions
    requiredAnnotations
    restrictions
    graphLookup

  -- That's all, folks!

----------------------------------------------------------------------
-- Inference
----------------------------------------------------------------------

inferPermissionsSCC :: [PermissionName]
           -> (Graph.Vertex -> (Node, FunctionName, [FunctionName]))
           -> (FunctionName -> Maybe Graph.Vertex)
           -> Tree.Tree Graph.Vertex
           -> IO ()
inferPermissionsSCC implicitPermissions graphLookup graphVertex scc = do
    -- We continue processing until the SCC's permission information reaches a
    -- fixed point, i.e., we are no longer adding permission information.
    growing <- newIORef mempty
    fix $ \ loop -> do
      writeIORef growing mempty

      -- For each function in the SCC:
      for_ scc $ \ vertex -> do
        let
          (node, name, _incoming) = graphLookup vertex
        -- For each permission action in function, plus implicit permissions not waived:
        permissionActions <- readIORef $ nodePermissions node
        let
          implicitPermissionActions =
            [ Need p
            | p <- implicitPermissions
            , not $ Waive p `HashSet.member` permissionActions
            ]
          nodePermissionActions = HashSet.toList permissionActions <> implicitPermissionActions
        nodeGrowing <- propagatePermissionsNode graphLookup (node, initialSite nodePermissionActions, name, graphVertex <$> nodeCalls node)
        modifyIORef' growing (<> nodeGrowing)

      -- We continue processing the current SCC if we're still propagating
      -- permission information between functions.
      do
        shouldContinue <- readIORef growing
        if getAny shouldContinue then loop else pure ()

propagatePermissionsNode :: (Graph.Vertex -> (Node, t1, t))
                       -> (Node, Site, FunctionName, CallSequence (Maybe Graph.Vertex))
                       -> IO Any
propagatePermissionsNode graphLookup (node, newInitialSite, _name, callVertices) = do
        let
          sites = nodeSites node

        -- We initialize the first call site of the function according to its
        -- permission actions.
        IOVector.modify sites (\old -> old \/ newInitialSite) 0

        -- Next, we infer information about permissions at each call site in the
        -- function by traversing its call tree.
        let

        -- We start processing the call tree from the root, filling in the list
        -- of top-level call sites for the function.
        processCallSequence graphLookup callVertices 0 sites

        initial <- IOVector.read sites 0
        final <- IOVector.read sites (IOVector.length sites - 1)
        permissionsFromCallSites (nodePermissions node) (initial, final)


processCallSequence :: (Graph.Vertex -> (Node, t1, t))
                    -> CallSequence (Maybe Graph.Vertex)
                    -> Int
                    -> IOVector Site
                    -> IO ()
processCallSequence graphLookup s i v =
  case viewlCallSequence s of
    Just (a,b) -> do
            processCallTree graphLookup a i v
            processCallSequence graphLookup b (succ i) v

            -- Once we've collected permission information for each call site
            -- and propagated it forward, we propagate all new or /newly-conflicting/
            -- information /backward/ through the whole sequence; this has the
            -- effect of filling in the 0th call site (before the first call) in
            -- a function with any relevant permissions from the body of the
            -- function.
            --
            -- FIXME: I think we could do this purely, because only the result
            -- at index 0 should matter at this point.
            --
            -- FIXME: We also get hear when we're at the head of a Choice
            -- (because it works in a copy of the vector), is this the right
            -- thing to do there?
            when (i == 0) $ do
              for_ (reverse [1 .. IOVector.length v - 1]) $ \ statement -> do
                after <- IOVector.read v statement
                flip (IOVector.modify v) (pred statement) $ \ before
                  -> before \/ permissionDiff after before
    Nothing -> pure ()
  where
    permissionDiff (PermissionPresenceSet x) (PermissionPresenceSet y) =
      PermissionPresenceSet (HashMap.differenceWith keepConflicting x y)
    keepConflicting pafter _pbefore =
      if conflicting pafter then Just pafter else Nothing

-- | Given a call tree and the permission presence set at the current site (as
-- an index into a vector of sites), update the current site and the following one
-- with the new permission presence set.
processCallTree :: (Graph.Vertex -> (Node, t1, t)) -- graphLookup
                -> CallTree (Maybe Graph.Vertex)   -- input
                -> Int                             -- offset within current sequence
                -> IOVector Site                   -- current sequence
                -> IO ()
processCallTree graphLookup (Choice a b) i v = do
            callsA <- IOVector.replicate (callSequenceLength a + 1) bottom
            callsB <- IOVector.replicate (callSequenceLength b + 1) bottom
            processCallSequence graphLookup a 0 callsA
            processCallSequence graphLookup b 0 callsB
            beforeA <- IOVector.read callsA 0
            afterA <- IOVector.read callsA (IOVector.length callsA - 1)
            beforeB <- IOVector.read callsB 0
            afterB <- IOVector.read callsB (IOVector.length callsB - 1)
            IOVector.write v i (beforeA \/ beforeB)
            IOVector.write v (succ i) (afterA \/ afterB)

processCallTree graphLookup (Call (Just call)) i v = do
            let (Node { nodePermissions = callPermissionsRef }, _, _) = graphLookup call
            callPermissions <- readIORef callPermissionsRef

            -- We propagate permissions forward in the call tree
            -- at each step. This ensures that the /final/ call site (after the
            -- last call) contains relevant permissions from the body of the
            -- function.
            IOVector.write v (succ i)
              =<< IOVector.read v i

            -- Update permission presence (has/lacks/conflicts) according to
            -- permission actions (needs/denies/grants/revokes).
            --
            for_ (HashSet.toList callPermissions) $ permissionsPresenceFromCalleeActions i v

processCallTree _ (Call Nothing) _ _ =
                -- Assume an unknown call has irrelevant permissions. I just know this
                -- is going to bite me later.
                pure ()



-- Note how this works with the forward-propagation above: if a call
-- site grants or revokes a permission for which information was
-- propagated from the previous call site, the old information is
-- /replaced/ to indicate the change in permissions; it doesn't
-- generate a conflict unless there's actually conflicting info. And
-- if some permission is irrelevant to a particular call, it just
-- passes on through.
permissionsPresenceFromCalleeActions :: Int
                                     -> IOVector Site
                                     -> PermissionAction
                                     -> IO ()
permissionsPresenceFromCalleeActions i v callPermission = do
  flip (IOVector.modify v) i $ \current -> current \/ getJoin (actionPrecondition callPermission)
  flip (IOVector.modify v) (succ i) $ strongUpdateCap callPermission

strongUpdateCap :: PermissionAction -> PermissionPresenceSet -> PermissionPresenceSet
strongUpdateCap act =
  let
    pn = permissionActionName act
  in modifyPresence pn update
  where
    (j, m) = actionPostcondition act
    update old = (old /\ m) \/ j

-- After processing a call tree, we can infer its permission actions
-- based on the permissions in the first and last call sites.
permissionsFromCallSites :: IORef PermissionActionSet -> (Site, Site) -> IO Any
permissionsFromCallSites permissionRef initialFinal@(initial, final) = do
            initialActions <- readIORef permissionRef
            let currentSize = HashSet.size initialActions

            -- For each "relevant" permission P in first & last call sites:
            let
              relevantPermissions = presenceKeys (initial \/ final)

            let
              derivedActions = HashSet.fromList $ mconcat $ map (derivePermissionActions initialFinal) relevantPermissions
              finalActions = derivedActions <> initialActions

            finalActions `seq` writeIORef permissionRef finalActions
            let
              modifiedSize = HashSet.size finalActions

            -- If we added permissions, the inferred set of permissions for this
            -- SCC may still be growing, so we re-process the SCC until we reach a
            -- fixed point.
            --
            -- TODO: Limit the number of iterations to prevent infinite loops.
            pure $ Any $ modifiedSize > currentSize

-- Given the initial and final call sites and a permission P, determine the action
-- of the function with respect to P by considering its presence or absence at function entry and exit.
derivePermissionActions :: (Site,Site) -> PermissionName -> [PermissionAction]
derivePermissionActions (initial,final) p =
  needsRevokes <> grants
  where
    -- (NB. The seemingly redundant side conditions here prevent spurious
    -- error messages from inconsistent permissions.)
    needsRevokes =
      if (has `leq` lookupPresence p initial && not (lacks `leq` lookupPresence p initial))
      then -- When the initial state has P, the function needs P.
        needs <> revokes
      else
        mempty
    grants =
      if (lacks `leq` lookupPresence p initial && not (has `leq` lookupPresence p initial))
         && (has `leq` lookupPresence p final && not (lacks `leq` lookupPresence p final))
      then
        -- When the initial state lacks P but the final state has P, the
        -- function grants P.
        [Grant p]
      else
        mempty

    needs = [Need p]
    revokes =
      if (lacks `leq` lookupPresence p final && not (has `leq` lookupPresence p final))
      then 
        -- When the initial state has P, but the final state lacks P,
        -- the function revokes P.
        [Revoke p]
      else
        mempty


----------------------------------------------------------------------
-- Reporting
----------------------------------------------------------------------


reportSCC
  :: [PermissionName]
  -> [(FunctionName, PermissionActionSet)]
  -> [Restriction]
  -> (Graph.Vertex -> (Node, FunctionName, [FunctionName]))
  -> Tree.Tree Graph.Vertex
  -> Logger ()
reportSCC implicitPermissions requiredAnnotations restrictions graphLookup scc = do
  -- For each function in each SCC:
    for_ scc $ \vertex -> do
      let
        (node, _name, _incoming) = graphLookup vertex
        name = nodeName node
        pos = nodePos node
        requiredPermissions = lookup name requiredAnnotations
      do
        (annotations, permissions) <- liftIO $ do
          a <- readIORef $ nodeAnnotations node
          p <- readIORef $ nodePermissions node
          return (a,p)
        reportDefinition
          implicitPermissions
          restrictions
          requiredPermissions
          (annotations, permissions, name, pos)
      do
        sites <- liftIO $ fmap Vector.toList $ Vector.freeze $ nodeSites node
        reportCallSites restrictions (sites, nodeCalls node, name, pos)


-- | Report violations at the function definition due to missing required annotations,
-- annotations that miss inferred permissions, or inconsistent inferred permissions.
reportDefinition
  :: [PermissionName]
  -> [Restriction]
  -> Maybe PermissionActionSet
  -> (PermissionActionSet, PermissionActionSet, FunctionName, NodeInfo)
  -> Logger ()
reportDefinition implicitPermissions _restrictions requiredPermissions (annotations, permissions, name, pos) = do

  -- If a function has required annotations, ensure the annotation mentions all
  -- inferred permissions. Implicit permissions don't need to be annotated.
  let implicit = HashSet.fromList $ map Need implicitPermissions
  for_ requiredPermissions $ \ userAnnotated -> do
    let
      requiredNotAnnotated = foldl' HashSet.difference permissions
        ([ userAnnotated
        , implicit
        ] :: [PermissionActionSet])
    unless (HashSet.null requiredNotAnnotated) $ do
      record True $ Error pos $ Text.concat
        [ "missing required annotation on '"
        , name
        , "'; annotation "
        , Text.pack $ show $ HashSet.toList userAnnotated
        , " is missing: "
        , Text.pack $ show $ HashSet.toList requiredNotAnnotated
        ]

  -- If a function has annotations...
  unless (HashSet.null annotations) $ do
    let
      inferredNotDeclared = foldl' HashSet.difference permissions
        ([ annotations
        , implicit
        ] :: [PermissionActionSet])

    -- ...then those annotations must mention all inferred permissions.
    unless (HashSet.null inferredNotDeclared) $ do
      record True $ Error pos $ Text.concat
        [ "annotation on '"
        , name
        , "' is missing these permissions: "
        , Text.pack $ show $ HashSet.toList inferredNotDeclared
        ]

    -- Likewise, the inferred permission actions must be consistent with the
    -- declared permission actions.
    for_ (HashSet.toList permissions) $ \ permission -> do
      let
        inconsistencies = case permission of
          Need p
            -> [Grant p | Grant p `HashSet.member` permissions]
            <> [Waive p | Waive p `HashSet.member` permissions]
            <> [Deny p | Deny p `HashSet.member` permissions]
          -- WTB disjunctive patterns
          Revoke p
            -> [Grant p | Grant p `HashSet.member` permissions]
            <> [Waive p | Waive p `HashSet.member` permissions]
            <> [Deny p | Deny p `HashSet.member` permissions]
          Grant p
            -> [Revoke p | Revoke p `HashSet.member` permissions]
            <> [Need p | Need p `HashSet.member` permissions]
          _ -> []
      for_ inconsistencies $ \ inconsistency -> do
        record True $ Error pos $ Text.concat
          [ "inferred inconsistent permissions for '"
          , name
          , "': "
          , Text.pack $ show permission
          , " is incompatible with "
          , Text.pack $ show inconsistency
          ]

-- | Report violations at the calls in the given function due to
-- callee functions with conflicting permissions or violated restrictions.
reportCallSites :: [Restriction]
                -> ([Site], CallSequence FunctionName, FunctionName, NodeInfo)
                -> Logger ()
reportCallSites restrictions (sites, callees, name, pos) = do
      -- Report call sites with conflicting information.
      let conflicts = getJoin $ foldMap (Join . conflictingPresence) sites
      unless (nullPresence conflicts) $ do
        record True $ Error pos $ Text.concat $
          [ "conflicting information for permissions "
          , Text.pack $ show $ presenceKeys conflicts
          , " in '"
          , name
          , "'"
          ]

      -- For each call site, check every restriction and report any violations.
      for_ (zip [0 :: Int ..] sites) $ \ (index, s) -> do
        let
          position = case index of
            0 -> ["before first call"]
            _ ->
              [ "at "
              , Text.pack $ show $ callSequenceIndex (index - 1) callees
              ]
        for_ restrictions $ \ restriction -> do
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


-- | Builds call graph edges from a list of functions, for input to
-- @Data.Graph.graphFromEdges@.
edgesFromFunctions :: [Function] -> IO [(Node, FunctionName, [FunctionName])]
edgesFromFunctions functions = do
  result <- newIORef []
  for_ functions $ \ function -> do
    let name = functionName function
    permissions <- newIORef $ functionPermissions function
    annotations <- newIORef $ functionPermissions function
    sites <- IOVector.replicate (callSequenceLength (functionCalls function) + 1) bottom
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

-- | Evaluates a restriction in a context.
evalRestriction :: PermissionPresenceSet -> Restriction -> Bool
evalRestriction context restriction
  | uses `leq` lookupPresence (restName restriction) context = go $ restExpression restriction -- FIXME: what if the context lacks the given restriction or uses it? ignore?
  | otherwise = True
  where
    go = \ case
      -- Since 'Conflicts' represents both 'Has' and 'Lacks', it matches both.
      Context pn pval -> pval `leq` lookupPresence pn context
      a `And` b -> go a && go b
      a `Or` b -> go a || go b
      Not a -> not $ go a

validatePermissionActionSet :: Config -> PermissionActionSet -> [PermissionName]
validatePermissionActionSet config =
  filter (not . permissionNameOk) . HashSet.toList {- n.b. removes duplicates -} . HashSet.map permissionActionName
  where
    ds = configDeclarations config
    permissionNameOk n = n `Map.member` ds

-- | @validatePermissions config calltree@ returns a list of logger entries for
-- each permission name that appears on explicit declarations in @calltree@
-- that was not declared in @config@
validatePermissions :: Config -> CallMap -> [Entry]
validatePermissions config =
  report
  . Map.filter (not . null . snd)
  . Map.map (\(ni,_callTree,actions) -> (ni, validatePermissionActionSet config actions))
  . getCallMap
  where
    report :: Map.Map Ident (NodeInfo, [PermissionName]) -> [Entry]
    report = map (\(i, (ni, names)) -> Warning ni (explain i names)) . Map.toList
    explain :: Ident -> [PermissionName] -> Text.Text
    explain i names =
      "The permissions of " <> (Text.pack $ show i)
      <> " [" <> commaSepText names <>  "]"
      <> " were not found in the config file, possible typos?"
    commaSepText :: [PermissionName] -> Text.Text
    commaSepText = Text.intercalate ", " . map (\(PermissionName txt) -> txt)

-- | Given a set of permission actions (either inferred or explicitly annotated)
-- compute the permission presense available on entry to the function.
initialSite :: [PermissionAction] -> Site
initialSite =
  getJoin . foldMap actionPrecondition

-- | Given the permission action of a call, return
-- the permission presence that must be true /prior/ to the call.
--
-- If a call needs, uses or revokes a permission, then it must have that
-- permission prior to the call.  (And if it uses it, it must indicate
-- that too).
-- If a call grants or denies a permission, then it must lack it prior to the call
actionPrecondition :: PermissionAction -> Join Site
actionPrecondition (Need p) = site p has
actionPrecondition (Use p) = site p (uses \/ has)
actionPrecondition (Revoke p) = site p has
actionPrecondition (Deny p) = site p lacks -- fixme: caller should drop use of p if the join results in a conflict
actionPrecondition (Grant p) = site p lacks -- fixme: caller should drop use of p if the join results in a conflic
actionPrecondition (Waive _p) = mempty

-- returns a pair (j, m) where the previous permission presense x will be modified to (x /\ m) \/ j.
-- so m should be top for a no-op, and j should be bottom for a no-op.
actionPostcondition :: PermissionAction -> (PermissionPresence, PermissionPresence)
actionPostcondition p =
  case p of
    Grant {} -> (has, uses) -- drop previous cap, keep usage, add Has
    Revoke {} -> (lacks, bottom) -- deletes previous cap and use, add Lacks
    _ -> (bottom, top) -- keep everything unchanged
  where
    top = uses \/ conflicts

-- | Convenience function for building call site info.
site :: PermissionName -> PermissionPresence -> Join Site
site pn = Join . singletonPresence pn

