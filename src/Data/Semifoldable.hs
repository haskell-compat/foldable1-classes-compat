{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

#if __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy                #-}
#endif

{-# OPTIONS_GHC -Wall -Werror #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semifoldable
-- Copyright   :  Edward Kmett
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- class of non-empty data structures that can be folded to a summary value.
--
-----------------------------------------------------------------------------

module Data.Semifoldable (
    Semifoldable(..),
    intercalate1,
    semifoldrM,
    semifoldlM,
    semifoldrMapM,
    semifoldlMapM,
    semimaximumBy,
    semiminimumBy,
    ) where

import Data.Foldable      (Foldable, foldlM, foldr)
import Data.List          (foldl, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
       (Dual (..), First (..), Last (..), Max (..), Min (..), Product (..),
       Semigroup (..), Sum (..))
import Prelude
       (Maybe (..), Monad (..), Ord, Ordering (..), error, id, seq, ($), ($!),
       (.), (=<<))

import qualified Data.List.NonEmpty as NE

#ifdef MIN_VERSION_generic_deriving
import Generics.Deriving.Base
       ((:*:) (..), (:+:) (..), (:.:) (..), M1 (..), Par1 (..), Rec1 (..), V1)
#else
import GHC.Generics
       ((:*:) (..), (:+:) (..), (:.:) (..), M1 (..), Par1 (..), Rec1 (..), V1)
#endif

#if MIN_VERSION_base(4,4,0)
import Data.Complex (Complex (..))
#endif

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down (..))
#endif

#if MIN_VERSION_base(4,8,0)
import qualified Data.Monoid as Mon
#endif

#if !MIN_VERSION_base(4,12,0)
import Data.Orphans ()
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged (Tagged (..))
#endif

-- Instances
import Control.Applicative.Backwards (Backwards (..))
import Control.Applicative.Lift      (Lift (..))
import Control.Monad.Trans.Identity  (IdentityT (..))
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Reverse          (Reverse (..))
import Data.Tree                     (Tree (..))

import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum     as Functor

-- coerce
#if __GLASGOW_HASKELL__ <708
import Unsafe.Coerce (unsafeCoerce)
#else
import Data.Coerce (Coercible, coerce)
#endif

#if __GLASGOW_HASKELL__ <708
coerce :: a -> b
coerce = unsafeCoerce

(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
#else
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
#endif

-- | Non-empty data structures that can be folded.
class Foldable t => Semifoldable t where
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL semifoldMap | semifoldrMap #-}
#endif

    -- At some point during design it was possible to define this class using
    -- only 'toNonEmpty'. But it seems a bad idea in general.
    --
    -- So currently we require either semifoldMap or semifoldrMap
    --
    -- * semifoldMap defined using semifoldrMap
    -- * semifoldrMap defined using semifoldMap
    --
    -- One can alsays define instance using following pattern:
    --
    --     toNonEmpty = ...
    --     foldMap f     = foldMap f     . toNonEmpty
    --     semifoldrMap f g = semifoldrMap f g . toNonEmpty

    -- | Combine the elements of a structure using a semigroup.
    semifold :: Semigroup m => t m -> m
    semifold = semifoldMap id

    -- | Map each element of the structure to a semigroup,
    -- and combine the results.
    --
    -- >>> semifoldMap Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    semifoldMap :: Semigroup m => (a -> m) -> t a -> m
    semifoldMap f = semifoldrMap f (\a m -> f a <> m)

    -- | A variant of 'semifoldMap' that is strict in the accumulator.
    --
    -- >>> semifoldMap' Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    semifoldMap' :: Semigroup m => (a -> m) -> t a -> m
    semifoldMap' f = semifoldl'Map f (\m a -> m <> f a)

    -- | Right-associative fold of a structure.
    --
    -- In the case of lists, 'semifoldr', when applied to a binary operator,
    -- and a list, reduces the list using the binary operator,
    -- from right to left:
    --
    -- > semifoldr f [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn1 `f` xn )...)
    --
    -- Note that, since the head of the resulting expression is produced by
    -- an application of the operator to the first element of the list,
    -- 'semifoldr' can produce a terminating expression from an infinite list.
    --
    -- For a general 'Semifoldable' structure this should be semantically identical
    -- to,
    --
    -- @semifoldr f = semifoldr f . 'toNonEmpty'@
    --
    semifoldr :: (a -> a -> a) -> t a -> a
    semifoldr = semifoldrMap id

    -- | Right-associative fold of a structure, but with strict application of
    -- the operator.
    --
    semifoldr' :: (a -> a -> a) -> t a -> a
    semifoldr' = semifoldr'Map id

    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, 'semifoldl', when applied to a binary
    -- operator, and a list, reduces the list using the binary operator,
    -- from left to right:
    --
    -- > semifoldl f [x1, x2, ..., xn] == (...((x1 `f` x2) `f`...) `f` xn
    --
    -- Note that to produce the outermost application of the operator the
    -- entire input list must be traversed. This means that 'semifoldl' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use 'semifoldl'' instead of 'semifoldl'. The reason for this is that latter does
    -- not force the "inner" results (e.g. @x1 \`f\` x2@ in the above example)
    -- before applying them to the operator (e.g. to @(\`f\` x3)@). This results
    -- in a thunk chain \(\mathcal{O}(n)\) elements long, which then must be
    -- evaluated from the outside-in.
    --
    -- For a general 'Semifoldable' structure this should be semantically identical
    -- to,
    --
    -- @semifoldl f z = semifoldl f . 'toNonEmpty'@
    --
    semifoldl :: (a -> a -> a) -> t a -> a
    semifoldl = semifoldlMap id

    -- | Left-associative fold of a structure but with strict application of
    -- the operator.
    --
    -- This ensures that each step of the fold is forced to weak head normal
    -- form before being applied, avoiding the collection of thunks that would
    -- otherwise occur. This is often what you want to strictly reduce a finite
    -- list to a single, monolithic result (e.g. 'length').
    --
    -- For a general 'Semifoldable' structure this should be semantically identical
    -- to,
    --
    -- @semifoldl' f z = semifoldl f . 'toNonEmpty'@
    --
    semifoldl' :: (a -> a -> a) -> t a -> a
    semifoldl' = semifoldl'Map id

    -- | List of elements of a structure, from left to right.
    --
    -- >>> toNonEmpty (Identity 2)
    -- 2 :| []
    --
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . semifoldMap singleton

    -- | The largest element of a non-empty structure.
    --
    -- >>> semimaximum (32 :| [64, 8, 128, 16])
    -- 128
    --
    semimaximum :: forall a . Ord a => t a -> a
    semimaximum = getMax . semifoldMap Max

    -- | The least element of a non-empty structure.
    --
    -- >>> semiminimum (32 :| [64, 8, 128, 16])
    -- 8
    --
    semiminimum :: forall a . Ord a => t a -> a
    semiminimum = getMin . semifoldMap Min

    -- | The first element of a non-empty structure.
    --
    -- >>> semihead (1 :| [2, 3, 4])
    -- 1
    --
    semihead :: t a -> a
    semihead = getFirst . semifoldMap First

    -- | The last element of a non-empty structure.
    --
    -- >>> semilast (1 :| [2, 3, 4])
    -- 4
    --
    semilast :: t a -> a
    semilast = getLast . semifoldMap Last

    -- | For 'Functor's, @'semifoldrMap' f g = semifoldr g . 'fmap' g@.
    semifoldrMap :: (a -> b) -> (a -> b -> b) -> t a -> b
    semifoldrMap f g xs =
        appFromMaybe (semifoldMap (FromMaybe #. h) xs) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g a b

    -- | For 'Functor's, @'semifoldl'Map' f g = semifoldl' g . 'fmap' g@.
    semifoldl'Map :: (a -> b) -> (b -> a -> b) -> t a -> b
    semifoldl'Map f g xs = semifoldrMap f' g' xs SNothing
      where
        -- g' :: a -> (SMaybe b -> b) -> SMaybe b -> b
        g' a x SNothing  = x $! SJust (f a)
        g' a x (SJust b) = x $! SJust (g b a)

        -- f' :: a -> SMaybe b -> b
        f' a SNothing  = f a
        f' a (SJust b) = g b a

    -- | For 'Functor's, @'semifoldlMap' f g = semifoldl g . 'fmap' g@.
    semifoldlMap :: (a -> b) -> (b -> a -> b) -> t a -> b
    semifoldlMap f g xs =
        -- foldlMap1 f g . toNonEmpty
        appFromMaybe (getDual (semifoldMap ((Dual . FromMaybe) #. h) xs)) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g b a

    -- | For 'Functor's, @'semifoldr'Map' f g = semifoldr' g . 'fmap' g@.
    semifoldr'Map :: (a -> b) -> (a -> b -> b) -> t a -> b
    semifoldr'Map f g xs = semifoldlMap f' g' xs SNothing
      where
        g' x a SNothing  = x $! SJust (f a)
        g' x a (SJust b) = x $! SJust (g a b)

        f' a SNothing  = f a
        f' a (SJust b) = g a b

-- Newtypes for semifoldrMap and semifoldlMap definitions.
-- c.f. Endo
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- Strict maybe, used to implement default semifoldl'Map etc.
data SMaybe a = SNothing | SJust !a

-- instances for Prelude types

instance Semifoldable NonEmpty where
    semifoldMap f (x :| xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

    semifoldMap' f (x :| xs) = foldl' (\m y -> m <> f y) (f x) xs

    toNonEmpty = id

    semifoldr f (x :| xs) = go x xs where
        go y [] = y
        go y (z : zs) = f y (go z zs)

    semifoldrMap g f (x :| xs) = go x xs where
        go y []       = g y
        go y (z : zs) = f y (go z zs)

    semifoldl f (x :| xs) = foldl f x xs
    semifoldlMap g f (x :| xs) = foldl f (g x) xs

    semifoldl' f (x :| xs) = foldl' f x xs
    semifoldl'Map g f (x :| xs) = foldl' f (g x) xs

    semihead = NE.head
    semilast = NE.last

instance Semifoldable ((,) a) where
    semifoldMap f (_, y) = f y
    semifoldr _ (_, y) = y
    toNonEmpty (_, x) = x :| []
    semiminimum (_, x) = x
    semimaximum (_, x) = x
    semihead (_, x) = x
    semilast (_, x) = x

instance Semifoldable Dual where
    semifoldMap = coerce

instance Semifoldable Sum where
    semifoldMap = coerce

instance Semifoldable Product where
    semifoldMap = coerce

instance Semifoldable Min where
    semifoldMap = coerce

instance Semifoldable Max where
    semifoldMap = coerce

instance Semifoldable First where
    semifoldMap = coerce

instance Semifoldable Last where
    semifoldMap = coerce

#if MIN_VERSION_base(4,6,0)
instance Semifoldable Down where
    semifoldMap = coerce
#endif

#if MIN_VERSION_base(4,8,0)
deriving instance (Semifoldable f) => Semifoldable (Mon.Alt f)
#endif

#if MIN_VERSION_base(4,12,0)
deriving instance (Semifoldable f) => Semifoldable (Mon.Ap f)
#endif

#if MIN_VERSION_base(4,4,0)
instance Semifoldable Complex where
    semifoldMap f (x :+ y) = f x <> f y

    toNonEmpty (x :+ y) = x :| y : []
#endif

-- Instances for GHC.Generics

instance Semifoldable V1 where
    semifoldMap _ x = x `seq` error "semifoldMap @V1"

instance Semifoldable Par1 where
    semifoldMap = coerce

deriving instance Semifoldable f => Semifoldable (Rec1 f)

deriving instance Semifoldable f => Semifoldable (M1 i c f)

instance (Semifoldable f, Semifoldable g) => Semifoldable (f :+: g) where
    semifoldMap f (L1 x) = semifoldMap f x
    semifoldMap f (R1 y) = semifoldMap f y

instance (Semifoldable f, Semifoldable g) => Semifoldable (f :*: g) where
    semifoldMap f (x :*: y) = semifoldMap f x <> semifoldMap f y

instance (Semifoldable f, Semifoldable g) => Semifoldable (f :.: g) where
    semifoldMap f = semifoldMap (semifoldMap f) . unComp1

-- | Insert an @m@ between each pair of @t m@.
--
-- >>> intercalate1 ", " $ "hello" :| ["how", "are", "you"]
-- "hello, how, are, you"
--
-- >>> intercalate1 ", " $ "hello" :| []
-- "hello"
--
-- >>> intercalate1 mempty $ "I" :| ["Am", "Fine", "You?"]
-- "IAmFineYou?"
--
intercalate1 :: (Semifoldable t, Semigroup m) => m -> t m -> m
intercalate1 sep = semifoldr (\a b -> a <> sep <> b)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
semifoldrM :: (Semifoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
semifoldrM = semifoldrMapM return

-- | Map variant of 'semifoldrM'.
semifoldrMapM :: (Semifoldable t, Monad m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
semifoldrMapM g f = go . toNonEmpty
  where
    go (e:|es) =
      case es of
        []   -> g e
        x:xs -> f e =<< go (x:|xs)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
semifoldlM :: (Semifoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
semifoldlM = semifoldlMapM return

-- | Map variant of 'semifoldlM'.
semifoldlMapM :: (Semifoldable t, Monad m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
semifoldlMapM g f t = g x >>= \y -> foldlM f y xs
  where x:|xs = toNonEmpty t

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
semimaximumBy :: Semifoldable t => (a -> a -> Ordering) -> t a -> a
semimaximumBy cmp = semifoldl max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
semiminimumBy :: Semifoldable t => (a -> a -> Ordering) -> t a -> a
semiminimumBy cmp = semifoldl min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-- NonEmptyDList

newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  xs <> ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL #. (:|)

-- | Convert a dlist to a non-empty list
runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($[]) . unNEDL
{-# INLINE runNonEmptyDList #-}

-------------------------------------------------------------------------------
-- Extra instances
-------------------------------------------------------------------------------

instance Semifoldable Identity where
    semifoldMap                = coerce

    semifoldr _                = coerce
    semifoldrMap g _           = coerce g
    semifoldl _                = coerce
    semifoldlMap g _           = coerce g

    toNonEmpty (Identity x) = x :| []

    semilast    = coerce
    semihead    = coerce
    semiminimum = coerce
    semimaximum = coerce

instance (Semifoldable f, Semifoldable g) => Semifoldable (Functor.Product f g) where
    semifoldMap f (Functor.Pair x y)    = semifoldMap f x <> semifoldMap f y
    semifoldrMap g f (Functor.Pair x y) = foldr f (semifoldrMap g f y) x

    semihead (Functor.Pair x _) = semihead x
    semilast (Functor.Pair _ y) = semilast y

instance (Semifoldable f, Semifoldable g) => Semifoldable (Functor.Sum f g) where
    semifoldMap f (Functor.InL x) = semifoldMap f x
    semifoldMap f (Functor.InR y) = semifoldMap f y

    semifoldr f (Functor.InL x) = semifoldr f x
    semifoldr f (Functor.InR y) = semifoldr f y

    semifoldrMap g f (Functor.InL x) = semifoldrMap g f x
    semifoldrMap g f (Functor.InR y) = semifoldrMap g f y

    toNonEmpty (Functor.InL x) = toNonEmpty x
    toNonEmpty (Functor.InR y) = toNonEmpty y

    semihead (Functor.InL x) = semihead x
    semihead (Functor.InR y) = semihead y
    semilast (Functor.InL x) = semilast x
    semilast (Functor.InR y) = semilast y

    semiminimum (Functor.InL x) = semiminimum x
    semiminimum (Functor.InR y) = semiminimum y
    semimaximum (Functor.InL x) = semimaximum x
    semimaximum (Functor.InR y) = semimaximum y

instance (Semifoldable f, Semifoldable g) => Semifoldable (Compose f g) where
    semifoldMap f = semifoldMap (semifoldMap f) . getCompose
    -- This is incorrect definition!
    -- semifoldrMap f g = semifoldrMap (semifoldrMap f g) g . getCompose

    semihead = semihead . semihead . getCompose
    semilast = semilast . semilast . getCompose

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Semifoldable Tree where
    semifoldMap f (Node x [])       = f x
    semifoldMap f (Node x (y : ys)) = f x <> semifoldMap (semifoldMap f) (y :| ys)

    semifoldMap' f = go where
        go (Node x ys) =
            foldl' (\m zs -> let gozs = go zs in gozs `seq` m <> gozs) (f x) ys

    -- This is incorrect definition!
    -- semifoldrMap f _ (Node x [])     = f x
    -- semifoldrMap f g (Node x (y:ys)) =
    --     g (f x) (semifoldrMap (semifoldrMap f g) g (y :| ys))

    semifoldlMap f g (Node x xs) = goForest (f x) xs where
        goForest = foldl' go
        go y (Node z zs) = goForest (g y z) zs

    semifoldl'Map f g (Node x xs) = goForest (f x) xs where
        goForest !y = foldl' go y
        go !y (Node z zs) = goForest (g y z) zs

    semihead (Node x _) = x

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance Semifoldable f => Semifoldable (Reverse f) where
    semifoldMap f = getDual . semifoldMap (Dual . f) . getReverse

    -- TODO:
    -- head1 = last1 . getReverse
    -- last1 = head1 . getReverse

deriving instance Semifoldable f => Semifoldable (IdentityT f)

instance Semifoldable f => Semifoldable (Backwards f) where
    semifoldMap f = semifoldMap f . forwards

instance Semifoldable f => Semifoldable (Lift f) where
    semifoldMap f (Pure x)  = f x
    semifoldMap f (Other y) = semifoldMap f y

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_tagged
instance Semifoldable (Tagged b) where
    semifoldMap                = coerce

    semifoldr _                = coerce
    semifoldrMap g _           = coerce g
    semifoldl _                = coerce
    semifoldlMap g _           = coerce g

    toNonEmpty (Tagged x) = x :| []

    semilast    = coerce
    semihead    = coerce
    semiminimum = coerce
    semimaximum = coerce
#endif

-- $setup
-- >>> import Prelude
