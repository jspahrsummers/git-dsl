{-# LANGUAGE GADTs #-}

module Expr ( Expr(..)
            ) where

import qualified Git

data Progress

data Expr a where
    ReadConfig :: String -> Expr String
    Remotes :: Expr [Git.Remote]
    Fetch :: Git.FetchOptions -> Git.Remote -> Expr Progress
    FindDefaultRemote :: [Git.Remote] -> Expr Git.Remote

gen :: Expr a -> String
gen (ReadConfig key) =
    "[[self\n" ++
        "\tloadConfiguration]\n" ++
        "\tmap:^(GTConfiguration *configuration) {\n" ++
            "\t\treturn [configuration stringForKey:\"" ++ key ++ "\"];\n" ++
        "\t}]\n"
