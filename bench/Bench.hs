{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.DeepSeq    (NFData (..))
import Criterion.Main
import Data.Foldable      (Foldable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semifoldable
import Data.Semigroup     (Min (..))
import Data.Tree          (Tree (..))

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
        [ bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) ne
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) ne
        , bench "semifoldl' min"       $ whnf (semifoldl' min) ne
        , bench "semifoldl min"        $ whnf (semifoldl min) ne
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) ne
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) ne
        ]
    , env (return $ NE1 input) $ \ne -> bgroup "NonEmpty-semifoldMap"
        [ bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) ne
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) ne
        , bench "semifoldl' min"       $ whnf (semifoldl' min) ne
        , bench "semifoldl min"        $ whnf (semifoldl min) ne
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) ne
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) ne
        ]
    , env (return $ NE3 input) $ \ne -> bgroup "NonEmpty-semifoldrMap"
        [ bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) ne
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) ne
        , bench "semifoldl' min"       $ whnf (semifoldl' min) ne
        , bench "semifoldl min"        $ whnf (semifoldl min) ne
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) ne
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) ne
        ]

    -- Trees
    , env (return tree) $ \tr -> bgroup "Tree-vanilla"
        [ bench "semihead" $ whnf semihead tr
        , bench "semilast" $ whnf semilast tr
        , bench "semimaximum" $ whnf semimaximum tr
        , bench "semimaximum'" $ whnf (semifoldl' max) tr

        , bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) tr
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) tr
        , bench "semifoldl' min"       $ whnf (semifoldl' min) tr
        , bench "semifoldl min"        $ whnf (semifoldl min) tr
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) tr
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) tr
        ]
    , env (return $ Tree1 tree) $ \tr -> bgroup "Tree-semifoldMap"
        [ bench "semihead" $ whnf semihead tr
        , bench "semilast" $ whnf semilast tr
        , bench "semimaximum" $ whnf semimaximum tr
        , bench "semimaximum'" $ whnf (semifoldl' max) tr

        , bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) tr
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) tr
        , bench "semifoldl' min"       $ whnf (semifoldl' min) tr
        , bench "semifoldl min"        $ whnf (semifoldl min) tr
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) tr
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) tr
        ]
    , env (return $ Tree3 tree) $ \tr -> bgroup "Tree-semifoldrMap"
        [ bench "semihead" $ whnf semihead tr
        , bench "semilast" $ whnf semilast tr
        , bench "semimaximum" $ whnf semimaximum tr
        , bench "semimaximum'" $ whnf (semifoldl' max) tr

        , bench "semifoldMap Min"      $ whnf (getMin . semifoldMap Min) tr
        , bench "semifoldMap' Min"     $ whnf (getMin . semifoldMap' Min) tr
        , bench "semifoldl' min"       $ whnf (semifoldl' min) tr
        , bench "semifoldl min"        $ whnf (semifoldl min) tr
        , bench "semifoldl'Map id min" $ whnf (semifoldl'Map id min) tr
        , bench "semifoldlMap id min"  $ whnf (semifoldlMap id min) tr
        ]
    ]

-------------------------------------------------------------------------------
-- NonEmpty variants
-------------------------------------------------------------------------------

-- Using semifoldMap only
newtype NE1 a = NE1 (NonEmpty a)
  deriving (Functor, Data.Foldable.Foldable)

instance NFData a => NFData (NE1 a) where
    rnf (NE1 xs) = rnf xs

instance Semifoldable NE1 where
    semifoldMap f (NE1 xs) = semifoldMap f xs

-- Using toNonEmpty
-- newtype NE2 a = NE2 (NonEmpty a)
--   deriving (Functor, Foldable)
--
-- instance NFData a => NFData (NE2 a) where
--     rnf (NE2 xs) = rnf xs
--
-- instance Semifoldable NE2 where
--     toNonEmpty (NE2 xs) = toNonEmpty xs

-- Using to semifoldrMap
newtype NE3 a = NE3 (NonEmpty a)
  deriving (Functor, Foldable)

instance NFData a => NFData (NE3 a) where
    rnf (NE3 xs) = rnf xs

instance Semifoldable NE3 where
    semifoldrMap g f (NE3 xs) = semifoldrMap g f xs

-------------------------------------------------------------------------------
-- Tree variants
-------------------------------------------------------------------------------

-- Using semifoldMap only
newtype Tree1 a = Tree1 (Tree a)
  deriving (Functor, Foldable)

instance NFData a => NFData (Tree1 a) where
    rnf (Tree1 xs) = rnf xs

instance Semifoldable Tree1 where
    semifoldMap f (Tree1 xs) = semifoldMap f xs

-- Using toNonEmpty
-- newtype Tree2 a = Tree2 (Tree a)
--   deriving (Functor, Foldable)
--
-- instance NFData a => NFData (Tree2 a) where
--     rnf (Tree2 xs) = rnf xs
--
-- instance Semifoldable Tree2 where
--     toNonEmpty (Tree2 xs) = toNonEmpty xs

-- Using to semifoldrMap
newtype Tree3 a = Tree3 (Tree a)
  deriving (Functor, Foldable)

instance NFData a => NFData (Tree3 a) where
    rnf (Tree3 xs) = rnf xs

instance Semifoldable Tree3 where
    semifoldrMap f g (Tree3 xs) = semifoldrMap f g xs
