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

generate :: Expr a -> Prelude.String
generate (Sequence a b) = "[" ++ generate a ++ " then:^{ return " ++ generate b ++ "; }]"
generate (Inject a b) = "[" ++ generate a ++ " concat:[" ++ generate b ++ " ignoreValues]]"
generate LoadConfig = "[self loadConfiguration]"
generate (ReadConfigKey key config) = "[RACSignal zip:@[ " ++ generate key ++ ", " ++ generate config ++ " ] reduce:^(NSString *key, GTConfiguration *configuration) { return [configuration stringForKey:key]; }]"
generate (ListRemotes config) = "[" ++ generate config ++ " map:^(GTConfiguration *configuration) { return configuration.remotes; }]"
-- TODO: Error if there are no remotes.
generate (DefaultRemote remotes) = "[" ++ generate remotes ++ " map:^(NSArray *remotes) { return remotes.firstObject; }]"
generate (Fetch remote) = "[" ++ generate remote ++ " flattenMap:^(GTRemote *remote) { return [self trackFetchProgressWithArguments:@[ @\"fetch\", @\"--progress\", @\"--prune\", @\"--recurse-submodules=on-demand\", remote.name ]]; }]"
generate CurrentBranch = "[self currentBranch]"
-- TODO: Take advantage of the signal stream.
generate LocalBranches = "[[self localBranches] collect]"
generate (SkipElement element array) = "[RACSignal zip:@[ " ++ generate array ++ ", " ++ generate element ++ " ] reduce:^(NSArray *array, id element) { return [array mtl_arrayByRemovingObject:element]; }]"
-- TODO
generate (FastForwardBranches branches) = undefined

fetchFromRemote :: Expr Progress
fetchFromRemote =
    let defaultRemote :: Expr Remote
        defaultRemote = DefaultRemote $ ListRemotes LoadConfig

        branchesWithoutCurrent :: Expr (Array Branch)
        branchesWithoutCurrent = SkipElement CurrentBranch LocalBranches
    in Inject (Fetch defaultRemote) (FastForwardBranches branchesWithoutCurrent)
