{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Expr ( Expr(..)
            ) where

import Data.Monoid
import Prelude hiding (String)
import qualified Prelude (String)

data Configuration
data String
data Array a
data Remote

data Expr a where
    Value :: Exportable a => a -> Expr a
    LoadConfig :: Expr Configuration
    ReadConfigKey :: Expr Configuration -> Expr String
    ListRemotes :: Expr Configuration -> Expr (Array Remote)

newtype Exported a = Exported Prelude.String

instance Functor Exported where
    fmap f (Exported str) = Exported $ f str

instance Monoid Exported where
    mempty = Exported mempty
    mappend a b = Exported $ mappend a b

class Exportable a where
    export :: a -> Exported a

instance Exportable Integer where
    export n = Exported $ "[RACSignal return:@(" ++ show n ++ ")]"

instance Exportable Double where
    export n = Exported $ "[RACSignal return:@(" ++ show n ++ ")]"

instance Exportable Prelude.String where
    export str = Exported $ "[RACSignal return:@\"" ++ str ++ "\"]"

generate :: Expr a -> Exported a
generate (Value v) = export v
generate LoadConfig = 
