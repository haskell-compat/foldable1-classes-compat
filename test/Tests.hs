{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Functor.Compose                 (Compose (..))
import Data.Functor.Identity                (Identity (..))
import Data.Functor.Product                 (Product (..))
import Data.Functor.Sum                     (Sum (..))
import Data.List.NonEmpty                   (NonEmpty (..))
import Data.Semigroup
       (First (..), Last (..), Max (..), Min (..), Semigroup (..))
import Data.Tree                            (Tree (..))
import Test.Framework.Providers.API         (Test, TestName, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Runners.Console       (defaultMain)
import Test.QuickCheck
       (Arbitrary, Fun, Property, Testable, applyFun, applyFun2,
       counterexample, mapSize, (===))
import Test.QuickCheck.Poly                 (A, B, OrdA)

import Test.QuickCheck.Instances ()

import Data.Foldable     (Foldable (foldMap), toList)
import Data.Semifoldable

main :: IO ()
main = defaultMain
    [ foldable1tests "NonEmpty" (P1 :: P1 NonEmpty)
    , foldable1tests "Tree"     (P1 :: P1 Tree)
    , foldable1tests "Identity" (P1 :: P1 Identity)
    , foldable1tests "Compose"  (P1 :: P1 (Compose NonEmpty NonEmpty))
    , foldable1tests "Product"  (P1 :: P1 (Product NonEmpty NonEmpty))
    , foldable1tests "Sum"      (P1 :: P1 (Sum NonEmpty NonEmpty))
    ]

-------------------------------------------------------------------------------
-- tests
-------------------------------------------------------------------------------

foldable1tests
    :: forall f.
      ( Semifoldable f
      , Arbitrary (f A), Show (f A)
      , Arbitrary (f OrdA), Show (f OrdA)
      , Arbitrary (f [B]), Show (f [B])
      )
    => TestName
    -> P1 f
    -> Test
foldable1tests name _p = testGroup name
    [ testProperty "semifoldMap ~= foldMap" coherentFoldMap
    , testProperty "toList . toNonEmpty ~= toList" coherentToNonEmpty

    , testProperty "semifoldl non/strict" $ smaller strictFoldl1
    , testProperty "semifoldr non/strict" $ smaller strictFoldr1
    , testProperty "semifoldlMap non/strict" $ smaller strictFoldlMap
    , testProperty "semifoldrMap non/strict" $ smaller strictFoldrMap

    -- test against default implementations
    , testProperty "semifoldMap default" defaultFoldMap
    , testProperty "semifoldrMap default" $ smaller semidefaultFoldrMap
    , testProperty "semifoldlMap default" $ smaller semidefaultFoldlMap
    , testProperty "toNonEmpty default" defaultToNonEmpty

    , testProperty "semihead default" defaultHead1
    , testProperty "semilast default" defaultLast1
    , testProperty "semiminimum default" defaultMinimum1
    , testProperty "semimaximum default" defaultMaximum1

    -- if we first convert to nonEmpty it should be the same
    , testProperty "foldMap via toNonEmpty" viaFoldMap
    , testProperty "semifoldr via toNonEmpty" $ smaller viaFoldr1
    , testProperty "semifoldl via toNonEmpty" $ smaller viaFoldl1
    , testProperty "semifoldr' via toNonEmpty" $ smaller viaFoldr1'
    , testProperty "semifoldl' via toNonEmpty" $ smaller viaFoldl1'
    , testProperty "semihead via toNonEmpty" viaHead1
    , testProperty "semilast via toNonEmpty" viaLast1
    , testProperty "semiminimum via toNonEmpty" viaMinimum1
    , testProperty "semimaximum via toNonEmpty" viaMaximum1
    ]
  where
    -- Things like Compose NonEmpty NonEmpty are big
    smaller :: Testable prop => prop -> Property
    smaller = mapSize (`div` 3)

    coherentFoldMap :: f A -> Fun A [B] -> Property
    coherentFoldMap xs f' = foldMap f xs === semifoldMap f xs where
        f = applyFun f'

    coherentToNonEmpty :: f A -> Property
    coherentToNonEmpty xs = toList (toNonEmpty xs) === toList xs

    strictFoldr1 :: f [B] -> Fun ([B], [B]) [B] -> Property
    strictFoldr1 xs g' = semifoldr g xs === semifoldr' g xs where
        g = applyFun2 g'

    strictFoldl1 :: f [B] -> Fun ([B], [B]) [B] -> Property
    strictFoldl1 xs g' = semifoldl g xs === semifoldl' g xs where
        g = applyFun2 g'

    strictFoldrMap :: f A -> Fun A [B] -> Fun (A, [B]) [B] -> Property
    strictFoldrMap xs f' g' = semifoldrMap f g xs === semifoldr'Map f g xs where
        f = applyFun f'
        g = applyFun2 g'

    strictFoldlMap :: f A -> Fun A [B] -> Fun ([B], A) [B] -> Property
    strictFoldlMap xs f' g' = semifoldlMap f g xs === semifoldl'Map f g xs where
        f = applyFun f'
        g = applyFun2 g'

    defaultFoldMap :: f A -> Fun A [B] -> Property
    defaultFoldMap xs f' = foldMap f xs === semifoldrMap f (\a m -> f a Data.Semigroup.<> m) xs where
        f = applyFun f'

    semidefaultFoldrMap :: f A -> Fun A [B] -> Fun (A, [B]) [B] -> Property
    semidefaultFoldrMap xs f' g'
        = counterexample ("NE: " ++ show ys)
        $ semifoldrMap f g xs === semifoldrMap f g ys
      where
        f = applyFun f'
        g = applyFun2 g'
        ys = toNonEmpty xs

    semidefaultFoldlMap :: f A -> Fun A [B] -> Fun ([B], A) [B] -> Property
    semidefaultFoldlMap xs f' g'
        = counterexample ("NE: " ++ show ys)
        $ semifoldlMap f g xs === semifoldlMap f g ys
      where
        f = applyFun f'
        g = applyFun2 g'
        ys = toNonEmpty xs

    defaultToNonEmpty :: f A -> Property
    defaultToNonEmpty xs = toNonEmpty xs === semifoldMap (:|[]) xs

    defaultHead1 :: f A -> Property
    defaultHead1 xs = semihead xs === getFirst (semifoldMap First xs)

    defaultLast1 :: f A -> Property
    defaultLast1 xs = semilast xs === getLast (semifoldMap Last xs)

    defaultMinimum1 :: f OrdA -> Property
    defaultMinimum1 xs = semiminimum xs === getMin (semifoldMap Min xs)

    defaultMaximum1 :: f OrdA -> Property
    defaultMaximum1 xs = semimaximum xs === getMax (semifoldMap Max xs)

    viaFoldMap :: f A -> Fun A [B] -> Property
    viaFoldMap xs f' = foldMap f xs === foldMap f (toNonEmpty xs) where
        f = applyFun f'

    viaFoldr1 :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldr1 xs g' = semifoldr g xs === semifoldr g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldr1' :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldr1' xs g' = semifoldr' g xs === semifoldr' g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldl1 :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldl1 xs g' = semifoldl g xs === semifoldl g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldl1' :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldl1' xs g' = semifoldl' g xs === semifoldl' g (toNonEmpty xs) where
        g = applyFun2 g'

    viaHead1 :: f A -> Property
    viaHead1 xs = semihead xs === semihead (toNonEmpty xs)

    viaLast1 :: f A -> Property
    viaLast1 xs = semilast xs === semilast (toNonEmpty xs)

    viaMinimum1 :: f OrdA -> Property
    viaMinimum1 xs = semiminimum xs === semiminimum (toNonEmpty xs)

    viaMaximum1 :: f OrdA -> Property
    viaMaximum1 xs = semimaximum xs === semimaximum (toNonEmpty xs)

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

-- Proxy of right kind
data P1 f
    = P1
    | Unused (f Int)
