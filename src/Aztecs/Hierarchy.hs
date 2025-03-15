{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.Hierarchy
  ( -- * Components
    Parent (..),
    Children (..),

    -- * Systems
    updateHierarchy,

    -- * Hierarchy
    Hierarchy (..),
    toList,
    foldWithKey,
    mapWithKey,
    mapWithAccum,

    -- ** Querying
    hierarchies,
    hierarchy,
    hierarchy',

    -- * Internal
    ParentState (..),
    ChildState (..),
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- | Parent component.
--
-- @since 0.3
newtype Parent = Parent {unParent :: EntityID}
  deriving (Eq, Ord, Show)

instance Component Parent

-- | Children component.
--
-- @since 0.3
newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Component Children

-- | System to update and maintain hierarchies of parents and children.
--
-- @since 0.3
updateHierarchy :: System (Access ())
updateHierarchy = do
  parents <- readQuery $ (,,) <$> entity <*> fetch <*> fetchMaybe
  children <- readQuery $ (,,) <$> entity <*> fetch <*> fetchMaybe
  return
    ( do
        mapM_
          ( \(e, Parent parent, maybeParentState) -> case maybeParentState of
              Just (ParentState parentState) -> do
                when (parent /= parentState) $ do
                  A.insert parent . bundle $ ParentState parent

                  -- Remove this entity from the previous parent's children.
                  maybeLastChildren <- A.lookup parentState
                  let lastChildren = maybe mempty unChildren maybeLastChildren
                  let lastChildren' = Set.filter (/= e) lastChildren
                  A.insert parentState . bundle . Children $ lastChildren'

                  -- Add this entity to the new parent's children.
                  maybeChildren <- A.lookup parent
                  let parentChildren = maybe mempty unChildren maybeChildren
                  A.insert parent . bundle . Children $ Set.insert e parentChildren
              Nothing -> do
                _ <- A.spawn . bundle $ ParentState parent
                maybeChildren <- A.lookup parent
                let parentChildren = maybe mempty unChildren maybeChildren
                A.insert parent . bundle . Children $ Set.insert e parentChildren
          )
          parents
        mapM_
          ( \(e, Children cs, maybeChildState) -> case maybeChildState of
              Just (ChildState childState) -> do
                when (cs /= childState) $ do
                  A.insert e . bundle $ ChildState cs
                  let added = Set.difference cs childState
                  -- TODO removed = Set.difference childState children
                  mapM_ (\e' -> A.insert e' . bundle . Parent $ e) added
              Nothing -> do
                A.insert e . bundle $ ChildState cs
                mapM_ (\e' -> A.insert e' . bundle . Parent $ e) cs
          )
          children
    )

-- | Hierarchy of entities.
--
-- @since 0.3
data Hierarchy a = Node
  { -- | Entity ID.
    --
    -- @since 0.3
    nodeEntityId :: EntityID,
    -- | Entity components.
    nodeEntity :: a,
    -- | Child nodes.
    --
    -- @since 0.3
    nodeChildren :: [Hierarchy a]
  }
  deriving (Functor)

-- | @since 0.9
instance Foldable Hierarchy where
  foldMap f n = f (nodeEntity n) <> foldMap (foldMap f) (nodeChildren n)

-- | @since 0.9
instance Traversable Hierarchy where
  traverse f n =
    Node (nodeEntityId n) <$> f (nodeEntity n) <*> traverse (traverse f) (nodeChildren n)

-- | Convert a hierarchy to a list of entity IDs and components.
--
-- @since 0.3
toList :: Hierarchy a -> [(EntityID, a)]
toList n = (nodeEntityId n, nodeEntity n) : concatMap toList (nodeChildren n)

-- | Fold a hierarchy with a function that takes the entity ID, entity, and accumulator.
--
-- @since 0.3
foldWithKey :: (EntityID -> a -> b -> b) -> Hierarchy a -> b -> b
foldWithKey f n b = f (nodeEntityId n) (nodeEntity n) (foldr (foldWithKey f) b (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID and entity.
--
-- @since 0.3
mapWithKey :: (EntityID -> a -> b) -> Hierarchy a -> Hierarchy b
mapWithKey f n =
  Node (nodeEntityId n) (f (nodeEntityId n) (nodeEntity n)) (map (mapWithKey f) (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID, entity, and accumulator.
--
-- @since 0.3
mapWithAccum :: (EntityID -> a -> b -> (c, b)) -> b -> Hierarchy a -> Hierarchy c
mapWithAccum f b n = case f (nodeEntityId n) (nodeEntity n) b of
  (c, b') -> Node (nodeEntityId n) c (map (mapWithAccum f b') (nodeChildren n))

-- | System to read a hierarchy of parents to children with the given query.
--
-- @since 0.3
hierarchy ::
  (Monad m) =>
  EntityID ->
  QueryT m a ->
  SystemT m (Maybe (Hierarchy a))
hierarchy e q = do
  children <- readQuery $ do
    e' <- entity
    cs <- fetch
    a <- q
    return (e', (unChildren cs, a))
  let childMap = Map.fromList children
  return $ hierarchy' e childMap

-- | Build all hierarchies of parents to children, joined with the given query.
--
-- @since 0.3
hierarchies ::
  (Monad m) =>
  QueryT m a ->
  SystemT m [Hierarchy a]
hierarchies q = do
  children <-
    readQuery
      ( do
          e <- entity
          cs <- fetch
          a <- q
          return (e, (unChildren cs, a))
      )
  let childMap = Map.fromList children
  roots <- query $ entity <* with @_ @Children <* without @_ @Parent
  return $ mapMaybe (`hierarchy'` childMap) roots

-- | Build a hierarchy of parents to children.
--
-- @since 0.3
hierarchy' :: EntityID -> Map EntityID (Set EntityID, a) -> Maybe (Hierarchy a)
hierarchy' e childMap = case Map.lookup e childMap of
  Just (cs, a) ->
    let bs = mapMaybe (`hierarchy'` childMap) (Set.toList cs)
     in Just
          Node
            { nodeEntityId = e,
              nodeEntity = a,
              nodeChildren = bs
            }
  Nothing -> Nothing

newtype ParentState = ParentState {unParentState :: EntityID}
  deriving (Show)

instance Component ParentState

newtype ChildState = ChildState {unChildState :: Set EntityID}
  deriving (Show)

instance Component ChildState
