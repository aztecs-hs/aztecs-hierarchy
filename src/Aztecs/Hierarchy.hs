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

import qualified Aztecs.ECS.Entities as E
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- | Parent component.
--
-- @since 0.3
newtype Parent = Parent {unParent :: E.Entity}
  deriving (Eq, Ord, Show)

-- | Children component.
--
-- @since 0.3
newtype Children = Children {unChildren :: Set E.Entity}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | System to update and maintain hierarchies of parents and children.
--
-- @since 0.3
-- TODO: Update this function to work with the new aztecs 0.13 API
updateHierarchy :: () -> ()
updateHierarchy = undefined

-- | Hierarchy of entities.
--
-- @since 0.3
data Hierarchy a = Node
  { -- | Entity ID.
    --
    -- @since 0.3
    nodeEntityId :: E.Entity,
    -- | Entity components.
    nodeEntity :: a,
    -- | Child nodes.
    --
    -- @since 0.3
    nodeChildren :: [Hierarchy a]
  }
  deriving (Show, Functor)

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
toList :: Hierarchy a -> [(E.Entity, a)]
toList n = (nodeEntityId n, nodeEntity n) : concatMap toList (nodeChildren n)

-- | Fold a hierarchy with a function that takes the entity ID, entity, and accumulator.
--
-- @since 0.3
foldWithKey :: (E.Entity -> a -> b -> b) -> Hierarchy a -> b -> b
foldWithKey f n b = f (nodeEntityId n) (nodeEntity n) (foldr (foldWithKey f) b (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID and entity.
--
-- @since 0.3
mapWithKey :: (E.Entity -> a -> b) -> Hierarchy a -> Hierarchy b
mapWithKey f n =
  Node (nodeEntityId n) (f (nodeEntityId n) (nodeEntity n)) (map (mapWithKey f) (nodeChildren n))

-- | Map a hierarchy with a function that takes the entity ID, entity, and accumulator.
--
-- @since 0.3
mapWithAccum :: (E.Entity -> a -> b -> (c, b)) -> b -> Hierarchy a -> Hierarchy c
mapWithAccum f b n = case f (nodeEntityId n) (nodeEntity n) b of
  (c, b') -> Node (nodeEntityId n) c (map (mapWithAccum f b') (nodeChildren n))

-- | System to read a hierarchy of parents to children with the given query.
--
-- @since 0.3
-- TODO: Update this function to work with the new aztecs 0.13 API
hierarchy ::
  (Monad m) =>
  E.Entity ->
  () ->
  m (Maybe (Hierarchy ()))
hierarchy = undefined

-- | Build all hierarchies of parents to children, joined with the given query.
--
-- @since 0.3
-- TODO: Update this function to work with the new aztecs 0.13 API
hierarchies ::
  (Monad m) =>
  () ->
  m [Hierarchy ()]
hierarchies = undefined

-- | Build a hierarchy of parents to children.
--
-- @since 0.3
hierarchy' :: E.Entity -> Map E.Entity (Maybe Children, a) -> Maybe (Hierarchy a)
hierarchy' e childMap = case Map.lookup e childMap of
  Just (cs, a) -> case cs of
    Just (Children cs') ->
      let bs = mapMaybe (`hierarchy'` childMap) (Set.toList cs')
       in Just
            Node
              { nodeEntityId = e,
                nodeEntity = a,
                nodeChildren = bs
              }
    Nothing ->
      Just
        Node
          { nodeEntityId = e,
            nodeEntity = a,
            nodeChildren = []
          }
  Nothing -> Nothing

newtype ParentState = ParentState {unParentState :: E.Entity}
  deriving (Show)

newtype ChildState = ChildState {unChildState :: Set E.Entity}
  deriving (Show)
