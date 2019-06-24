{-# LANGUAGE UndecidableInstances #-}

module RecursionSchemes where

import Control.Arrow

newtype Fix f = Fix { unfix :: f (Fix f) }

-- The Eq and Show instances are taken from
-- https://hackage.haskell.org/package/unification-fd-0.6.0/src/src/Data/Functor/Fixedpoint.hs,
-- where there's also some explanation of why we need
-- UndecidableInstances

instance (Eq (f (Fix f))) => Eq (Fix f) where
    Fix x == Fix y  =  x == y
    Fix x /= Fix y  =  x /= y

instance (Show (f (Fix f))) => Show (Fix f) where
    show (Fix f) = show f

cata :: Functor f => (f a -> a) -> Fix f -> a
cata fn = unfix >>> fmap (cata fn) >>> fn
