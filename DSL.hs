{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module DSL ( Expr(..)
           , Configuration
           , DSL.String
           , Array
           , Remote
           , Progress
           , Branch
           ) where

import Control.Arrow
import Control.Category
import Data.Monoid

data Configuration
data String
data Array a
data Remote
data Progress
data Branch

data Expr a b where
    Immediate :: (a -> b) -> Expr a b
    Sequence :: Expr a b -> Expr b c -> Expr a c
    ApplyFirst :: Expr a b -> Expr (a, c) (b, c)
    Inject :: Expr a b -> Expr () () -> Expr a b
    LoadConfig :: Expr () Configuration
    ReadConfigKey :: Expr (DSL.String, Configuration) DSL.String
    ListRemotes :: Expr Configuration (Array Remote)
    DefaultRemote :: Expr (Array Remote) Remote
    Fetch :: Expr Remote Progress
    CurrentBranch :: Expr () Branch
    LocalBranches :: Expr () (Array Branch)
    SkipElement :: Expr (a, Array a) (Array a)
    FastForwardBranches :: Expr (Array Branch) ()

instance Category Expr where
    id = Immediate Prelude.id
    b . a = Sequence a b

instance Arrow Expr where
    arr f = Immediate f
    first expr = ApplyFirst expr

fetchFromRemote :: Expr () Progress
fetchFromRemote = proc _ -> do
    defaultRemote <- LoadConfig >>> ListRemotes >>> DefaultRemote -< ()
    branchesWithoutCurrent <- SkipElement <<< (CurrentBranch &&& LocalBranches) -< ()

    progress <- Fetch -< defaultRemote
    FastForwardBranches -< branchesWithoutCurrent

    returnA -< progress
