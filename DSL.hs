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

infixr 0 <*
infixr 0 *>
infixl 8 >-
infixr 8 -<

(*>) :: Expr a -> Expr b -> Expr b
(*>) = Sequence

(<*) :: Expr a -> Expr b -> Expr a
(<*) = Inject

(>-) :: Expr a -> (Expr a -> Expr b) -> Expr b
a >- f = f a

(-<) = flip (>-)

deriving instance Show (Expr a)

fetchFromRemote :: Expr Progress
fetchFromRemote =
    let fastForward = LocalBranches
                        >- SkipElement CurrentBranch
                        >- FastForwardBranches
    in LoadConfig
        >- ListRemotes
        >- DefaultRemote
        >- Fetch
        <* fastForward
