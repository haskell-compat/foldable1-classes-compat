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
-- Module      :  Data.Foldable1
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

module Data.Foldable1 (
    Foldable1(..),
    intercalate1,
    foldrM1,
    foldlM1,
    maximum1By,
    minimum1By,
    ) where

import Data.Foldable      (Foldable, foldlM, foldr)
import Data.List          (foldl, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
       (Dual (..), First (..), Last (..), Max (..), Min (..), Product (..),
       Semigroup (..), Sum (..))
import Prelude
       (Maybe (..), Monad (..), Ord, Ordering (..), id, map, seq, ($), ($!),
       (.), (=<<))

import qualified Data.List.NonEmpty as NE

#if MIN_VERSION_base(4,4,0)
import Data.Complex (Complex (..))
import GHC.Generics
       ((:*:) (..), (:+:) (..), (:.:) (..), M1 (..), Par1 (..), Rec1 (..), V1)
import Prelude      (error)
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

-- Instances
import Control.Applicative.Backwards (Backwards (..))
import Control.Applicative.Lift      (Lift (..))
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
class Foldable t => Foldable1 t where
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL foldMap1 | foldr1Map #-}
#endif

    -- At some point during design it was possible to define this class using
    -- only 'toNonEmpty'. But it seems a bad idea in general.
    --
    -- So currently we require either foldMap1 or foldr1Map
    --
    -- * foldMap1 defined using foldr1Map
    -- * foldr1Map defined using foldMap1
    --
    -- One can alsays define instance using following pattern:
    --
    --     toNonEmpty = ...
    --     foldMap f     = foldMap f     . toNonEmpty
    --     foldr1Map f g = foldr1Map f g . toNonEmpty

    -- | Combine the elements of a structure using a semigroup.
    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    -- | Map each element of the structure to a semigroup,
    -- and combine the results.
    --
    -- >>> foldMap1 Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m
    foldMap1 f = foldr1Map f (<>)

    -- | A variant of 'foldMap1' that is strict in the accumulator.
    --
    -- >>> foldMap1' Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    foldMap1' :: Semigroup m => (a -> m) -> t a -> m
    foldMap1' f = foldl1'Map f (<>)

    -- | Right-associative fold of a structure.
    --
    -- In the case of lists, 'foldr1', when applied to a binary operator,
    -- and a list, reduces the list using the binary operator,
    -- from right to left:
    --
    -- > foldr1 f [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn1 `f` xn )...)
    --
    -- Note that, since the head of the resulting expression is produced by
    -- an application of the operator to the first element of the list,
    -- 'foldr1' can produce a terminating expression from an infinite list.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldr1 f = foldr1 f . 'toNonEmpty'@
    --
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 = foldr1Map id

    -- | Right-associative fold of a structure, but with strict application of
    -- the operator.
    --
    foldr1' :: (a -> a -> a) -> t a -> a
    foldr1' = foldr1'Map id

    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, 'foldl1', when applied to a binary
    -- operator, and a list, reduces the list using the binary operator,
    -- from left to right:
    --
    -- > foldl1 f [x1, x2, ..., xn] == (...((x1 `f` x2) `f`...) `f` xn
    --
    -- Note that to produce the outermost application of the operator the
    -- entire input list must be traversed. This means that 'foldl1' will
    -- diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to
    -- use 'foldl1'' instead of 'foldl1'. The reason for this is that latter does
    -- not force the "inner" results (e.g. @x1 \`f\` x2@ in the above example)
    -- before applying them to the operator (e.g. to @(\`f\` x3)@). This results
    -- in a thunk chain \(\mathcal{O}(n)\) elements long, which then must be
    -- evaluated from the outside-in.
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldl1 f z = foldl1 f . 'toNonEmpty'@
    --
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 = foldl1Map id

    -- | Left-associative fold of a structure but with strict application of
    -- the operator.
    --
    -- This ensures that each step of the fold is forced to weak head normal
    -- form before being applied, avoiding the collection of thunks that would
    -- otherwise occur. This is often what you want to strictly reduce a finite
    -- list to a single, monolithic result (e.g. 'length').
    --
    -- For a general 'Foldable1' structure this should be semantically identical
    -- to,
    --
    -- @foldl1' f z = foldl1 f . 'toNonEmpty'@
    --
    foldl1' :: (a -> a -> a) -> t a -> a
    foldl1' = foldl1'Map id

    -- | List of elements of a structure, from left to right.
    --
    -- >>> toNonEmpty (Identity 2)
    -- 2 :| []
    --
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . foldMap1 singleton

    -- | The largest element of a non-empty structure.
    --
    -- >>> maximum1 (32 :| [64, 8, 128, 16])
    -- 128
    --
    maximum1 :: forall a . Ord a => t a -> a
    maximum1 = getMax . foldMap1 Max

    -- | The least element of a non-empty structure.
    --
    -- >>> minimum1 (32 :| [64, 8, 128, 16])
    -- 8
    --
    minimum1 :: forall a . Ord a => t a -> a
    minimum1 = getMin . foldMap1 Min

    -- | The first element of a non-empty structure.
    --
    -- >>> head1 (1 :| [2, 3, 4])
    -- 1
    --
    head1 :: t a -> a
    head1 = getFirst . foldMap1 First

    -- | The last element of a non-empty structure.
    --
    -- >>> last1 (1 :| [2, 3, 4])
    -- 4
    --
    last1 :: t a -> a
    last1 = getLast . foldMap1 Last

    -- | For 'Functor's, @'foldr1Map' f g = foldr1 g . 'fmap' g@.
    foldr1Map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldr1Map f g xs =
        -- foldr1Map f g . toNonEmpty
        appFromMaybe (foldMap1 (FromMaybe #. h) xs) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g (f a) b

    -- | For 'Functor's, @'foldl1'Map' f g = foldl1' g . 'fmap' g@.
    foldl1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1'Map f g xs = foldr1Map f' (\x y z -> y $! SJust (x z)) xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g b (f a)

    -- | For 'Functor's, @'foldl1Map' f g = foldl1 g . 'fmap' g@.
    foldl1Map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1Map f g xs =
        -- foldl1Map f g . toNonEmpty
        appFromMaybe (getDual (foldMap1 ((Dual . FromMaybe) #. h) xs)) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g b (f a)

    -- | For 'Functor's, @'foldr1'Map' f g = foldr1' g . 'fmap' g@.
    foldr1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldr1'Map f g xs = foldl1Map f' (\x y z -> x $! SJust (y z)) xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g (f a) b

-- Newtypes for foldr1Map and foldl1Map definitions.
-- c.f. Endo
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- Strict maybe, used to implement default foldl1'Map etc.
data SMaybe a = SNothing | SJust !a

-- instances for Prelude types

instance Foldable1 NonEmpty where
    foldMap1 f (x :| xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

    foldMap1' f (x :| xs) = foldl' (\m y -> m <> f y) (f x) xs

    toNonEmpty = id

    foldr1 f (x :| xs) = go x xs where
        go y [] = y
        go y (z : zs) = f y (go z zs)

    foldr1Map g f (x :| xs) = go x xs where
        go y [] = g y
        go y (z : zs) = f (g y) (go z zs)

    foldl1 f (x :| xs) = foldl f x xs
    foldl1Map g f (x :| xs) = foldl f (g x) (map g xs)

    foldl1' f (x :| xs) = foldl' f x xs
    foldl1'Map g f (x :| xs) = foldl' f (g x) (map g xs)

    head1 = NE.head
    last1 = NE.last

instance Foldable1 ((,) a) where
    foldMap1 f (_, y) = f y
    foldr1 _ (_, y) = y
    toNonEmpty (_, x) = x :| []
    minimum1 (_, x) = x
    maximum1 (_, x) = x
    head1 (_, x) = x
    last1 (_, x) = x

instance Foldable1 Dual where
    foldMap1 = coerce

instance Foldable1 Sum where
    foldMap1 = coerce

instance Foldable1 Product where
    foldMap1 = coerce

instance Foldable1 Min where
    foldMap1 = coerce

instance Foldable1 Max where
    foldMap1 = coerce

instance Foldable1 First where
    foldMap1 = coerce

instance Foldable1 Last where
    foldMap1 = coerce

#if MIN_VERSION_base(4,6,0)
instance Foldable1 Down where
    foldMap1 = coerce
#endif

#if MIN_VERSION_base(4,8,0)
deriving instance (Foldable1 f) => Foldable1 (Mon.Alt f)
#endif

#if MIN_VERSION_base(4,12,0)
deriving instance (Foldable1 f) => Foldable1 (Mon.Ap f)
#endif

#if MIN_VERSION_base(4,4,0)
instance Foldable1 Complex where
    foldMap1 f (x :+ y) = f x <> f y

    toNonEmpty (x :+ y) = x :| y : []
#endif

-- Instances for GHC.Generics

#if MIN_VERSION_base(4,4,0)
instance Foldable1 V1 where
    foldMap1 _ x = x `seq` error "foldMap1 @V1"

instance Foldable1 Par1 where
    foldMap1 = coerce

deriving instance Foldable1 f => Foldable1 (Rec1 f)

deriving instance Foldable1 f => Foldable1 (M1 i c f)

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
    foldMap1 f (L1 x) = foldMap1 f x
    foldMap1 f (R1 y) = foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :*: g) where
    foldMap1 f (x :*: y) = foldMap1 f x <> foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :.: g) where
    foldMap1 f = foldMap1 (foldMap1 f) . unComp1
#endif

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
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 sep = foldr1 (\a b -> a <> sep <> b)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 f = go . toNonEmpty
  where
    go (e:|es) =
      case es of
        []   -> return e
        x:xs -> f e =<< go (x:|xs)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 f t = foldlM f x xs
  where x:|xs = toNonEmpty t

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
maximum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
maximum1By cmp = foldl1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
minimum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimum1By cmp = foldl1 min'
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

instance Foldable1 Tree where
    foldMap1 f (Node x [])       = f x
    foldMap1 f (Node x (y : ys)) = f x <> foldMap1 (foldMap1 f) (y :| ys)

    foldMap1' f = go where
        go (Node x ys) =
            foldl' (\m zs -> let gozs = go zs in gozs `seq` m <> gozs) (f x) ys

    -- This is incorrect definition!
    -- foldr1Map f _ (Node x [])     = f x
    -- foldr1Map f g (Node x (y:ys)) =
    --     g (f x) (foldr1Map (foldr1Map f g) g (y :| ys))

    foldl1Map f g (Node x xs) = goForest (f x) xs where
        goForest = foldl' go
        go y (Node z zs) = goForest (g y (f z)) zs

    foldl1'Map f g (Node x xs) = goForest (f x) xs where
        goForest !y = foldl' go y
        go !y (Node z zs) = goForest (g y (f z)) zs

    head1 (Node x _) = x

instance Foldable1 Identity where
    foldMap1                = coerce

    foldr1 _                = coerce
    foldr1Map g _           = coerce g
    foldl1 _                = coerce
    foldl1Map g _           = coerce g

    toNonEmpty (Identity x) = x :| []

    last1    = coerce
    head1    = coerce
    minimum1 = coerce
    maximum1 = coerce

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
    foldMap1 f (Functor.Pair x y)    = foldMap1 f x <> foldMap1 f y
    foldr1Map g f (Functor.Pair x y) = foldr (f . g) (foldr1Map g f y) x

    head1 (Functor.Pair x _) = head1 x
    last1 (Functor.Pair _ y) = last1 y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
    foldMap1 f (Functor.InL x) = foldMap1 f x
    foldMap1 f (Functor.InR y) = foldMap1 f y

    foldr1 f (Functor.InL x) = foldr1 f x
    foldr1 f (Functor.InR y) = foldr1 f y

    foldr1Map g f (Functor.InL x) = foldr1Map g f x
    foldr1Map g f (Functor.InR y) = foldr1Map g f y

    toNonEmpty (Functor.InL x) = toNonEmpty x
    toNonEmpty (Functor.InR y) = toNonEmpty y

    head1 (Functor.InL x) = head1 x
    head1 (Functor.InR y) = head1 y
    last1 (Functor.InL x) = last1 x
    last1 (Functor.InR y) = last1 y

    minimum1 (Functor.InL x) = minimum1 x
    minimum1 (Functor.InR y) = minimum1 y
    maximum1 (Functor.InL x) = maximum1 x
    maximum1 (Functor.InR y) = maximum1 y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose
    -- This is incorrect definition!
    -- foldr1Map f g = foldr1Map (foldr1Map f g) g . getCompose

    head1 = head1 . head1 . getCompose
    last1 = last1 . last1 . getCompose

instance Foldable1 f => Foldable1 (Reverse f) where
    foldMap1 f = getDual . foldMap1 (Dual . f) . getReverse

    -- TODO:
    -- head1 = last1 . getReverse
    -- last1 = head1 . getReverse

instance Foldable1 f => Foldable1 (Backwards f) where
    foldMap1 f = foldMap1 f . forwards

instance Foldable1 f => Foldable1 (Lift f) where
    foldMap1 f (Pure x)  = f x
    foldMap1 f (Other y) = foldMap1 f y

-- $setup
-- >>> import Prelude hiding (foldr1, foldl1)
