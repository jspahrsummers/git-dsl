module Git ( Remote(..)
           , URL(..)
           , FetchOptions(..)
           ) where

data URL = HTTPSURL String
         | GitURL String
         | SSHURL String
         deriving (Eq, Ord, Show)

data Remote = Remote String URL
            deriving (Eq, Ord, Show)

data FetchOptions = FetchOptions
    { prune :: Bool
    , recurseSubmodules :: Bool
    } deriving (Eq, Show)
