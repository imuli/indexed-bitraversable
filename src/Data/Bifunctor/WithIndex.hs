{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Copyright   : Public Domain
Description : Bifunctor with an index argument.
Maintainer  : Tristan <tplumb@commmonedge.com>
-}

module Data.Bifunctor.WithIndex
  ( BifunctorWithIndex(..)
  , ifirst
  , isecond
    -- * helpers for implementing 'BifunctorWithIndex'
  , ibimapConstant
  , ibimapVia
  ) where

import           Data.Bifunctor (Bifunctor, bimap)
import           Data.Functor.Const (Const)

-- | 'Bifunctor' with an extra index argument.
--
-- Instances must satisfy the transformed 'Bifunctor' laws:
--
-- @
-- ibimap (const id) (const id) ≡ id
-- ibimap f g . ibimap h j ≡ ibimap (\i -> f i . h i) (\i -> g i . j i)
-- @
class Bifunctor f => BifunctorWithIndex i f | f -> i where
  -- | Map over both arguments at the same time, given an index.
  ibimap :: (i -> a -> s) -> (i -> b -> t) -> f a b -> f s t

instance BifunctorWithIndex () Either where
  ibimap = ibimapConstant ()

instance BifunctorWithIndex () Const where
  ibimap = ibimapConstant ()

instance BifunctorWithIndex () (,) where
  ibimap = ibimapConstant ()

instance BifunctorWithIndex a ((,,) a) where
  ibimap = ibimapVia (\(a,_,_) -> a)

instance BifunctorWithIndex (a,b) ((,,,) a b) where
  ibimap = ibimapVia (\(a,b,_,_) -> (a,b))

instance BifunctorWithIndex (a,b,c) ((,,,,) a b c) where
  ibimap = ibimapVia (\(a,b,c,_,_) -> (a,b,c))

instance BifunctorWithIndex (a,b,c,d) ((,,,,,) a b c d) where
  ibimap = ibimapVia (\(a,b,c,d,_,_) -> (a,b,c,d))

-- | Map only over the first type argument, with an index.
ifirst :: BifunctorWithIndex i f => (i -> a -> s) -> f a b -> f s b
ifirst = flip ibimap (const id)

-- | Map only over the second type argument, with an index.
isecond :: BifunctorWithIndex i f => (i -> b -> t) -> f a b -> f a t
isecond = ibimap (const id)

-- | Always supply the same value as the index to 'ibimap', using 'bimap'.
ibimapConstant :: Bifunctor f => i -> (i -> a -> s) -> (i -> b -> t) -> f a b -> f s t
ibimapConstant i f g = bimap (f i) (g i)

-- | Supply a value derived from the overall structure as the index to 'ibimap', using 'bimap'.
ibimapVia :: Bifunctor f => (f a b -> i) -> (i -> a -> s) -> (i -> b -> t) -> f a b -> f s t
ibimapVia via f g x = let i = via x in bimap (f i) (g i) x
