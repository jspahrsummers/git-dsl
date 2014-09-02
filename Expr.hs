{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Expr ( Expr(..)
            ) where

import Prelude ()

data Configuration
data String

data Expr a where
    Value :: a -> Expr a
    LoadConfig :: Expr Configuration
    ReadConfigKey :: Expr Configuration -> Expr String


instance Functor Expr where
    fmap f (Value a) = Value $ f a
    fmap f GetRepository = Value $ f Repository
    fmap f GetConfiguration =

forGreatJustice :: Expr Configuration
forGreatJustice = GetRepository *> GetConfiguration

instance Category Expr where
    id = Identity
    b . a = Compose a b

newtype GeneratedExpr a = GeneratedExpr String
    deriving (Eq, Show)

runGeneratedExpr :: GeneratedExpr a -> String
runGeneratedExpr (GeneratedExpr str) = str

apply :: Expr a b -> GeneratedExpr a -> GeneratedExpr b
apply Empty _ = GeneratedExpr ""
apply (StringExpr str) _ = GeneratedExpr $ "[RACSignal return:@\"" ++ str ++ "\"]"
apply Identity input = input
apply (Compose first second) input = GeneratedExpr $ "[" ++ runGeneratedExpr (apply first input) ++ " flattenMap:^(id input) { return " ++ runGeneratedExpr (apply second $ GeneratedExpr "input")
apply GetRepository _ = GeneratedExpr $ "self"
apply GetConfiguration repo = GeneratedExpr $ "[" ++ runGeneratedExpr repo ++ " loadConfiguration]"

doThing = GetRepository >>> GetConfiguration

{-
newtype GeneratedExpr = String

generate :: Expr a b -> State GeneratedExpr ()
generate (Value a) =
generate Identity =
-}

{-
data Expr a where
    Value :: a -> Expr a
    GetRepository :: Expr Repository

instance Functor Expr where
    fmap f (Value a) = Value $ f a
    fmap f GetRepository = Value $ f Repository

instance Applicative Expr where
    pure = Value

    (Value f) <*> (Value a) = Value $ f a
    (Value f) <*> GetRepository = Value $ f Repository

instance Monad Expr where
    return = pure

    (Value a) >>= f = f a
    GetRepository >>= f = Repository

repository :: Expr Repository
repository = GetRepository

configuration :: Repository -> Expr Configuration

valueForKey :: Configuration -> String -> Expr ConfigValue

remotes :: Configuration -> Expr RemoteList
-}




{-
data Expr a b where
    Value :: Gen v => v -> Expr () v
    Identity :: Expr a a
    Compose :: Expr a b -> Expr b c -> Expr a c

instance Category Expr where
    id = Identity
    bc . ab = Compose ab bc

data Context = Context
    { repository :: Repository
    , input :: Identifier
    } deriving (Eq, Show)

class Gen a where
    generate :: a -> Reader Context String

newtype Identifier = Identifier String

instance Gen Identifier where
    generate x = return x

instance Gen Integer where
    generate n = return "[RACSignal return:@(" ++ show n ++ ")]"

instance Gen Double where
    generate n = return "[RACSignal return:@(" ++ show n ++ ")]"

instance Gen String where
    generate s = return "[RACSignal return:@\"" ++ s ++ "\"]"

instance Gen (Expr a b) where
    generate (Value v) = generate v
    generate Identity = generate . input <$> ask
    generate (Compose ea eb) = do
        ia <- input <$> ask
        expr <- withReader { input = Identifier "value" } $ generate eb
        return $ "[" ++ ia ++ " flattenMap:^(id value) { return " ++ expr ++ " }]"

-}
