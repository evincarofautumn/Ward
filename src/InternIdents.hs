{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InternIdents (InternM, runInternM, internCallMap) where

import Types
import Control.Monad.Trans.State.Strict
import Language.C.Data.Ident (Ident)
import qualified Data.Map.Strict as M

newtype InternM b a = InternM (State (M.Map b b) a)
  deriving (Functor, Applicative, Monad)

runInternM :: Ord b => InternM b a -> a
runInternM (InternM m) = evalState m mempty

intern :: Ord b => b -> InternM b b
intern x = InternM $ do
  m <- get
  case M.lookup x m of
    Just x' -> pure x'
    Nothing -> put (M.insert x x m) >> pure x

internCallMap :: CallMap -> InternM Ident CallMap
internCallMap =
    fmap (CallMap . M.fromList) . mapM f . M.assocs . getCallMap
  where
    f (k, (nodeinfo, calltree, pas)) = do
      k' <- intern k
      calltree' <- traverse intern calltree
      pure (k', (nodeinfo, calltree', pas))
