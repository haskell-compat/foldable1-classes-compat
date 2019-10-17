{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.DeepSeq    (NFData (..))
import Criterion.Main
import Data.Foldable      (Foldable)
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty (..))
import Data.Tree          (Tree (..))
import Prelude            hiding (foldl1)

input :: NonEmpty Int
input = 1 :| take 10000000 [2 .. ]

tree :: Tree Int
tree = go 7 0 where
    go :: Int -> Int -> Tree Int
    go n x
        | n <= 0    = Node x []
        | otherwise = Node x [ go (pred n) (x * 10 + x') | x' <- [0 .. 9] ]

main :: IO ()
main = defaultMain
    -- NonEmpty left folds
    [ env (return input) $ \ne -> bgroup "NonEmpty-vanilla"
        [ bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne
        , bench "foldl1map id min"  $ whnf (foldl1map id min) ne
        ]
    , env (return $ NE1 input) $ \ne -> bgroup "NonEmpty-foldMap1"
        [ bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne
        , bench "foldl1map id min"  $ whnf (foldl1map id min) ne
        ]
   -- , env (return $ NE2 input) $ \ne -> bgroup "NonEmpty-toNonEmpty"
   --     [ bench "foldl1' min"       $ whnf (foldl1' min) ne
   --     , bench "foldl1 min"        $ whnf (foldl1 min) ne
   --     , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne
   --     , bench "foldl1map id min"  $ whnf (foldl1map id min) ne
   --     ]
    , env (return $ NE3 input) $ \ne -> bgroup "NonEmpty-foldr1map"
        [ bench "foldl1' min"       $ whnf (foldl1' min) ne
        , bench "foldl1 min"        $ whnf (foldl1 min) ne
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne
        , bench "foldl1map id min"  $ whnf (foldl1map id min) ne
        ]

    -- Trees
    , env (return tree) $ \tr -> bgroup "Tree-vanilla"
        [ bench "head1" $ whnf head1 tr
        , bench "last1" $ whnf last1 tr
        , bench "maximum1" $ whnf maximum1 tr
        , bench "maximum1'" $ whnf (foldl1' max) tr

        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) tr
        , bench "foldl1map id min"  $ whnf (foldl1map id min) tr
        ]
    , env (return $ Tree1 tree) $ \tr -> bgroup "Tree-foldMap1"
        [ bench "head1" $ whnf head1 tr
        , bench "last1" $ whnf last1 tr
        , bench "maximum1" $ whnf maximum1 tr
        , bench "maximum1'" $ whnf (foldl1' max) tr

        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) tr
        , bench "foldl1map id min"  $ whnf (foldl1map id min) tr
        ]
   -- , env (return $ Tree2 tree) $ \tr -> bgroup "Tree-toNonEmpty"
   --     [ bench "head1" $ whnf head1 tr
   --     , bench "last1" $ whnf last1 tr
   --     , bench "maximum1" $ whnf maximum1 tr
   --     , bench "maximum1'" $ whnf (foldl1' max) tr
   --     , bench "foldl1' min"       $ whnf (foldl1' min) tr
   --     , bench "foldl1 min"        $ whnf (foldl1 min) tr
   --     , bench "foldl1'map id min" $ whnf (foldl1'map id min) tr
   --     , bench "foldl1map id min"  $ whnf (foldl1map id min) tr
   --     ]
    , env (return $ Tree3 tree) $ \tr -> bgroup "Tree-foldr1map"
        [ bench "head1" $ whnf head1 tr
        , bench "last1" $ whnf last1 tr
        , bench "maximum1" $ whnf maximum1 tr
        , bench "maximum1'" $ whnf (foldl1' max) tr

        , bench "foldl1' min"       $ whnf (foldl1' min) tr
        , bench "foldl1 min"        $ whnf (foldl1 min) tr
        , bench "foldl1'map id min" $ whnf (foldl1'map id min) tr
        , bench "foldl1map id min"  $ whnf (foldl1map id min) tr
        ]
    ]

-------------------------------------------------------------------------------
-- NonEmpty variants
-------------------------------------------------------------------------------

-- Using foldMap1 only
newtype NE1 a = NE1 (NonEmpty a)
  deriving (Functor, Data.Foldable.Foldable)

instance NFData a => NFData (NE1 a) where
    rnf (NE1 xs) = rnf xs

instance Foldable1 NE1 where
    foldMap1 f (NE1 xs) = foldMap1 f xs

-- Using toNonEmpty
-- newtype NE2 a = NE2 (NonEmpty a)
--   deriving (Functor, Foldable)
-- 
-- instance NFData a => NFData (NE2 a) where
--     rnf (NE2 xs) = rnf xs
-- 
-- instance Foldable1 NE2 where
--     toNonEmpty (NE2 xs) = toNonEmpty xs

-- Using to foldr1map
newtype NE3 a = NE3 (NonEmpty a)
  deriving (Functor, Foldable)

instance NFData a => NFData (NE3 a) where
    rnf (NE3 xs) = rnf xs

instance Foldable1 NE3 where
    foldr1map g f (NE3 xs) = foldr1map g f xs

-------------------------------------------------------------------------------
-- Tree variants
-------------------------------------------------------------------------------

-- Using foldMap1 only
newtype Tree1 a = Tree1 (Tree a)
  deriving (Functor, Foldable)

instance NFData a => NFData (Tree1 a) where
    rnf (Tree1 xs) = rnf xs

instance Foldable1 Tree1 where
    foldMap1 f (Tree1 xs) = foldMap1 f xs

-- Using toNonEmpty
-- newtype Tree2 a = Tree2 (Tree a)
--   deriving (Functor, Foldable)
-- 
-- instance NFData a => NFData (Tree2 a) where
--     rnf (Tree2 xs) = rnf xs
-- 
-- instance Foldable1 Tree2 where
--     toNonEmpty (Tree2 xs) = toNonEmpty xs

-- Using to foldr1map
newtype Tree3 a = Tree3 (Tree a)
  deriving (Functor, Foldable)

instance NFData a => NFData (Tree3 a) where
    rnf (Tree3 xs) = rnf xs

instance Foldable1 Tree3 where
    foldr1map f g (Tree3 xs) = foldr1map f g xs
