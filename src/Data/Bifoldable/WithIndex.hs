{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Copyright   : Public Domain
Description : Bifoldable with an index argument.
Maintainer  : Tristan <tplumb@commmonedge.com>
-}

module Data.Bifoldable.WithIndex
  ( BifoldableWithIndex(..)
  , ibifoldl'
    -- * 'Data.Bitraversable.WithIndex.BitraverseWithIndex's that ignore results.
  , ibitraverse_
  , itraversefirst_
  , itraversesecond_
  , ibifor_
  , iforfirst_
  , iforsecond_
    -- * Helpers for implementing 'BifunctorWithIndex'.
  , ibifoldMapConstant
  , ibifoldMapVia
  ) where

import           Data.Bifoldable (Bifoldable, bifoldMap)
import           Data.Composition ((.*))
import           Data.Functor.Const (Const)
import           Data.Monoid (Endo(Endo), appEndo)

-- | 'Bifoldable' with an extra index argument.
--
-- Instances must satisfy the transformed 'Bifoldable'-'Bifunctor' laws:
--
-- @
-- ibifoldMap f g ≡ bifold . ibimap f g
-- ibifoldMap f g . ibimap h j ≡ ibifoldMap (\i -> f i . h i) (\i -> g i . j i)
-- @
class Bifoldable f => BifoldableWithIndex i f | f -> i where
  {-# MINIMAL ibifoldMap | ibifoldr #-}

  -- | Combine the elements of a structure, given ways of mapping them, with and index, to a monoid.
  ibifoldMap :: Monoid m => (i -> a -> m) -> (i -> b -> m) -> f a b -> m
  ibifoldMap f g = ibifoldr ((<>) .* f) ((<>) .* g) mempty

  -- | 'Data.Bifoldable.bifoldr' with an extra index argument.
  ibifoldr :: (i -> a -> c -> c) -> (i -> b -> c -> c) -> c -> f a b -> c
  ibifoldr f g z t = appEndo (ibifoldMap (Endo .* f) (Endo .* g) t) z

-- | 'Data.Bifoldable.bifoldl'' with an extra index argument.
ibifoldl' :: BifoldableWithIndex i f => (i -> c -> a -> c) -> (i -> c -> b -> c) -> c -> f a b -> c
ibifoldl' f g z0 xs = ibifoldr f' g' id xs z0 where
  f' i x k z = k $! f i z x
  g' i x k z = k $! g i z x

-- | A version of 'Data.Bitraversable.WithIndex.itraversefirst which ignores the results of the actions.
itraversefirst_ :: (BifoldableWithIndex i f, Applicative m) => (i -> a -> m s) -> f a b -> m ()
itraversefirst_ = flip ibitraverse_ (const $ const $ pure ())

-- | A version of 'Data.Bitraversable.WithIndex.itraversesecond which ignores the results of the actions.
itraversesecond_ :: (BifoldableWithIndex i f, Applicative m) => (i -> b -> m s) -> f a b -> m ()
itraversesecond_ = ibitraverse_ (const $ const $ pure ())

-- | A version of 'Data.Bitraversable.WithIndex.ibitraverse' which ignores the results of the actions.
--
-- This evaluates the actions with ascending indexes.
ibitraverse_ :: (BifoldableWithIndex i f, Applicative m) => (i -> a -> m s) -> (i -> b -> m t) -> f a b -> m ()
ibitraverse_ f g = ibifoldr ((*>) .* f) ((*>) .* g) (pure ())

-- | A version of 'Data.Bitraversable.WithIndex.ibitraverse' which ignores the results of the actions.
--
-- This evaluates the actions with ascending indexes.
ibifor_ :: (BifoldableWithIndex i f, Applicative m) => f a b -> (i -> a -> m s) -> (i -> b -> m t) -> m ()
ibifor_ x f g = ibitraverse_ f g x

-- | A version of 'Data.Bitraversable.WithIndex.iforfirst which ignores the results of the actions.
iforfirst_ :: (BifoldableWithIndex i f, Applicative m) => f a b -> (i -> a -> m s) -> m ()
iforfirst_ x = flip (ibifor_ x) (const $ const $ pure ())

-- | A version of 'Data.Bitraversable.WithIndex.iforsecond which ignores the results of the actions.
iforsecond_ :: (BifoldableWithIndex i f, Applicative m) => f a b -> (i -> b -> m s) -> m ()
iforsecond_ x = ibifor_ x (const $ const $ pure ())

-- | Always supply the same value as the index to 'ibifoldMap', using 'bifoldMap'.
ibifoldMapConstant :: (Monoid m, Bifoldable f) => i -> (i -> a -> m) -> (i -> b -> m) -> f a b -> m
ibifoldMapConstant i f g = bifoldMap (f i) (g i)

-- | Supply a value derived from the overall structure as the index to 'ibifoldMap', using 'bifoldMap'.
ibifoldMapVia :: (Monoid m, Bifoldable f) => (f a b -> i) -> (i -> a -> m) -> (i -> b -> m) -> f a b -> m
ibifoldMapVia via f g x = let i = via x in bifoldMap (f i) (g i) x

instance BifoldableWithIndex () Either where
  ibifoldMap = ibifoldMapConstant ()

instance BifoldableWithIndex () Const where
  ibifoldMap = ibifoldMapConstant ()

instance BifoldableWithIndex () (,) where
  ibifoldMap = ibifoldMapConstant ()

instance BifoldableWithIndex a ((,,) a) where
  ibifoldMap = ibifoldMapVia (\(a,_,_) -> a)

instance BifoldableWithIndex (a,b) ((,,,) a b) where
  ibifoldMap = ibifoldMapVia (\(a,b,_,_) -> (a,b))

instance BifoldableWithIndex (a,b,c) ((,,,,) a b c) where
  ibifoldMap = ibifoldMapVia (\(a,b,c,_,_) -> (a,b,c))

instance BifoldableWithIndex (a,b,c,d) ((,,,,,) a b c d) where
  ibifoldMap = ibifoldMapVia (\(a,b,c,d,_,_) -> (a,b,c,d))
