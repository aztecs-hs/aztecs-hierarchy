{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.Hierarchy where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

newtype Parent = Parent {unParent :: EntityID}
  deriving (Eq, Ord, Show)

instance Component Parent

newtype ParentState = ParentState {unParentState :: EntityID}
  deriving (Show)

instance Component ParentState

newtype Children = Children {unChildren :: Set EntityID}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Component Children

newtype ChildState = ChildState {unChildState :: Set EntityID}
  deriving (Show)

instance Component ChildState

update :: System (Access ())
update = do
  parents <- readQuery $ (,,) <$> entity <*> fetch @_ @Parent <*> fetchMaybe @_ @ParentState
  children <- readQuery $ (,,) <$> entity <*> fetch @_ @Children <*> fetchMaybe @_ @ChildState
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
