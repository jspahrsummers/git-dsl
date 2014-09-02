module Generator ( Generator(..)
                 ) where

import qualified DSL

class Generator g where
    generate :: g -> DSL.Expr () a -> Maybe String
