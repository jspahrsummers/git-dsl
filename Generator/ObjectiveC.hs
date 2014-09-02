{-# LANGUAGE GADTs #-}

module Generator.ObjectiveC ( ObjectiveC(..)
                            ) where

import DSL
import Generator

data ObjectiveC = ObjectiveC

instance Generator ObjectiveC where
    generate g (Sequence a b) = do
        ga <- generate g a
        gb <- generate g b
        return $ "[" ++ ga ++ " then:^{ return " ++ gb ++ "; }]"

    generate g (Inject a b) = do
        ga <- generate g a
        gb <- generate g b
        return $ "[" ++ ga ++ " concat:[" ++ gb ++ " ignoreValues]]"

    generate g LoadConfig = return "[self loadConfiguration]"

    generate g (ReadConfigKey key config) = do
        gk <- generate g key
        gc <- generate g config
        return $ "[RACSignal zip:@[ " ++ gk ++ ", " ++ gc ++ " ] reduce:^(NSString *key, GTConfiguration *configuration) { return [configuration stringForKey:key]; }]"

    generate g (ListRemotes config) = do
        gc <- generate g config
        return $ "[" ++ gc ++ " map:^(GTConfiguration *configuration) { return configuration.remotes; }]"

    -- TODO: Error if there are no remotes.
    generate g (DefaultRemote remotes) = do
        gr <- generate g remotes
        return $ "[" ++ gr ++ " map:^(NSArray *remotes) { return remotes.firstObject; }]"

    generate g (Fetch remote) = do
        gr <- generate g remote
        return $ "[" ++ gr ++ " flattenMap:^(GTRemote *remote) { return [self trackFetchProgressWithArguments:@[ @\"fetch\", @\"--progress\", @\"--prune\", @\"--recurse-submodules=on-demand\", remote.name ]]; }]"

    generate g CurrentBranch = return "[self currentBranch]"

    -- TODO: Take advantage of the signal stream.
    generate g LocalBranches = return "[[self localBranches] collect]"

    generate g (SkipElement element array) = do
        ga <- generate g array
        ge <- generate g element
        return $ "[RACSignal zip:@[ " ++ ga ++ ", " ++ ge ++ " ] reduce:^(NSArray *array, id element) { return [array mtl_arrayByRemovingObject:element]; }]"

    -- TODO
    generate g (FastForwardBranches branches) = return ""
