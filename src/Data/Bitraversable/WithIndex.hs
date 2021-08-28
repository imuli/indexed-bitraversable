{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Copyright   : Public Domain
Description : Bitraversable with an index argument.
Maintainer  : Tristan <tplumb@commmonedge.com>
-}

module Data.Bitraversable.WithIndex
  ( BitraversableWithIndex(..)
  , itraversefirst
  , itraversesecond
  , ibifor
  , iforfirst
  , iforsecond
    -- * helpers for implementing 'BitraversableWithIndex'
  , ibitraverseConstant
  , ibitraverseVia
    -- * helpers for implementing other classes
  , bitraverseHelper
  , ibimapHelper
  , ibifoldMapHelper
  ) where

import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.Composition ((.*))
import           Data.Functor.Const (Const(Const), getConst)
import           Data.Functor.Identity (Identity(Identity), runIdentity)

-- | 'Bitraversable' with an extra index argument.
--
-- Instances must satisfy the transformed 'Bitraversable' laws:
--
-- @
-- 'ibitraverse' (t '.*' f) (t '.*' g) ≡ t .* ibitraverse f g
-- 'ibitraverse' ('const' 'Identity') ('const' 'Identity') ≡ 'Identity'
-- Compose . fmap (ibitraverse f g) . ibitraverse (h j) ≡ ibitraverse (Compose . imap h . f) (Compose . imap g . g)
-- @
class Bitraversable f => BitraversableWithIndex i f | f -> i where
  -- | Evaluate the relevant functions at each element, providing the index
  -- into the structure to run any actions.
  --
  -- To ignore results, see 'Data.Foldable.WithIndex.ibitraverse_'.
  ibitraverse :: Applicative m => (i -> a -> m s) -> (i -> b -> m t) -> f a b -> m (f s t)

instance BitraversableWithIndex () Either where
  ibitraverse = ibitraverseConstant ()

instance BitraversableWithIndex () Const where
  ibitraverse = ibitraverseConstant ()

instance BitraversableWithIndex () (,) where
  ibitraverse = ibitraverseConstant ()

instance BitraversableWithIndex a ((,,) a) where
  ibitraverse = ibitraverseVia (\(a,_,_) -> a)

instance BitraversableWithIndex (a,b) ((,,,) a b) where
  ibitraverse = ibitraverseVia (\(a,b,_,_) -> (a,b))

instance BitraversableWithIndex (a,b,c) ((,,,,) a b c) where
  ibitraverse = ibitraverseVia (\(a,b,c,_,_) -> (a,b,c))

instance BitraversableWithIndex (a,b,c,d) ((,,,,,) a b c d) where
  ibitraverse = ibitraverseVia (\(a,b,c,d,_,_) -> (a,b,c,d))

-- | Traverse only over the first type argument, with an index.
itraversefirst :: (BitraversableWithIndex i f, Applicative m) => (i -> a -> m s) -> f a b -> m (f s b)
itraversefirst = flip ibitraverse (const pure)

-- | Traverse only over the second type argument, with an index.
itraversesecond :: (BitraversableWithIndex i f, Applicative m) => (i -> b -> m t) -> f a b -> m (f a t)
itraversesecond = ibitraverse (const pure)

-- | 'ibitraverse' with the structure as the first argument.
--
-- To ignore results, see 'Data.Foldable.WithIndex.ibifor_'.
ibifor :: (BitraversableWithIndex i f, Applicative m) => f a b -> (i -> a -> m s) -> (i -> b -> m t) -> m (f s t)
ibifor x f g = ibitraverse f g x

-- | Traverse only over the first type argument, with an index, passing the structure as the first argument.
iforfirst :: (BitraversableWithIndex i f, Applicative m) => f a b -> (i -> a -> m s) -> m (f s b)
iforfirst = flip itraversefirst

-- | Traverse only over the second type argument, with an index, passing the structure as the second argument.
iforsecond :: (BitraversableWithIndex i f, Applicative m) => f a b -> (i -> b -> m t) -> m (f a t)
iforsecond = flip itraversesecond

-- * Helpers for implementing other 'BitraversableWithIndex'.

-- | Always supply the same value as the index to 'ibitraverse', using 'bitraverse'
ibitraverseConstant :: (Bitraversable f, Applicative m) => i -> (i -> a -> m s) -> (i -> b -> m t) -> f a b -> m (f s t)
ibitraverseConstant i f g = bitraverse (f i) (g i)

-- | Supply a value derived from the overall structure as the index to 'ibitraverse', using 'bitraverse'.
ibitraverseVia :: (Bitraversable f, Applicative m) => (f a b -> i) -> (i -> a -> m s) -> (i -> b -> m t) -> f a b -> m (f s t)
ibitraverseVia via f g x = let i = via x in bitraverse (f i) (g i) x

-- * Helpers for implementing other classes.

-- | Implement 'Bitraversable' via 'BitraversableWithIndex'.
bitraverseHelper :: (BitraversableWithIndex i f, Applicative m) => (a -> m s) -> (b -> m t) -> f a b -> m (f s t)
bitraverseHelper f g = ibitraverse (const f) (const g)

-- | Implement 'Data.Bifunctor.WithIndex.ibimap' via 'BitraversableWithIndex'.
ibimapHelper :: BitraversableWithIndex i f => (i -> a -> s) -> (i -> b -> t) -> f a b -> f s t
ibimapHelper f g = runIdentity . ibitraverse (Identity .* f) (Identity .* g)

-- | Implement 'Data.Bifoldable.WithIndex.ibifoldMap' via 'BitraversableWithIndex'.
ibifoldMapHelper :: (BitraversableWithIndex i f, Monoid m) => (i -> a -> m) -> (i -> b -> m) -> f a b -> m
ibifoldMapHelper f g = getConst . ibitraverse (Const .* f) (Const .* g)
