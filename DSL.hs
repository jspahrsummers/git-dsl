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

import Data.Monoid

data Configuration
data String
data Array a
data Remote
data Progress
data Branch

data Expr a where
    Sequence :: Expr a -> Expr b -> Expr b
    Inject :: Expr a -> Expr b -> Expr a
    LoadConfig :: Expr Configuration
    ReadConfigKey :: Expr DSL.String -> Expr Configuration -> Expr DSL.String
    ListRemotes :: Expr Configuration -> Expr (Array Remote)
    DefaultRemote :: Expr (Array Remote) -> Expr Remote
    Fetch :: Expr Remote -> Expr Progress
    CurrentBranch :: Expr Branch
    LocalBranches :: Expr (Array Branch)
    SkipElement :: Expr a -> Expr (Array a) -> Expr (Array a)
    FastForwardBranches :: Expr (Array Branch) -> Expr ()

deriving instance Show (Expr a)

fetchFromRemote :: Expr Progress
fetchFromRemote =
    let defaultRemote :: Expr Remote
        defaultRemote = DefaultRemote $ ListRemotes LoadConfig

        branchesWithoutCurrent :: Expr (Array Branch)
        branchesWithoutCurrent = SkipElement CurrentBranch LocalBranches
    in Inject (Fetch defaultRemote) (FastForwardBranches branchesWithoutCurrent)
