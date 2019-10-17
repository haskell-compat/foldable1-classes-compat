{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable1
import Criterion.Main
import Prelude hiding (foldl1)

input :: NonEmpty Int
input = 1 :| take 10000000 [2 .. ]

main :: IO ()
main = defaultMain
    $ return
    $ env (return (input, toNE1 input, toNE2 input, toNE3 input)) $ \ ~(ne, ne1, ne2, ne3) -> bgroup "Left Folds"
        [ bgroup "NonEmpty"
            [ bench "foldl1' min"       $ whnf (foldl1' min) ne
            , bench "foldl1 min"        $ whnf (foldl1 min) ne
            , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne
            , bench "foldl1map id min"  $ whnf (foldl1map id min) ne
            ]
        , bgroup "foldMap1"
            [ bench "foldl1' min"       $ whnf (foldl1' min) ne1
            , bench "foldl1 min"        $ whnf (foldl1 min) ne1
            , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne1
            , bench "foldl1map id min"  $ whnf (foldl1map id min) ne1
            ]
        , bgroup "toNonEmpty"
            [ bench "foldl1' min"       $ whnf (foldl1' min) ne2
            , bench "foldl1 min"        $ whnf (foldl1 min) ne2
            , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne2
            , bench "foldl1map id min"  $ whnf (foldl1map id min) ne2
            ]
        , bgroup "foldr1map"
            [ bench "foldl1' min"       $ whnf (foldl1' min) ne3
            , bench "foldl1 min"        $ whnf (foldl1 min) ne3
            , bench "foldl1'map id min" $ whnf (foldl1'map id min) ne3
            , bench "foldl1map id min"  $ whnf (foldl1map id min) ne3
            ]
        ]

-- Using foldMap1 only
data NE1 a = a :* [a]
  deriving (Functor, Foldable)

instance NFData a => NFData (NE1 a) where
    rnf (x :* xs) = rnf x `seq` rnf xs

toNE1 :: NonEmpty a -> NE1  a
toNE1 (x :| xs) = x :* xs

instance Foldable1 NE1 where
    foldMap1 f (x :* xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

-- Using to nonEmpty
data NE2 a = a :** [a]
  deriving (Functor, Foldable)

instance NFData a => NFData (NE2 a) where
    rnf (x :** xs) = rnf x `seq` rnf xs

toNE2 :: NonEmpty a -> NE2  a
toNE2 (x :| xs) = x :** xs

instance Foldable1 NE2 where
    toNonEmpty (x :** xs) = x :| xs

-- Using to foldr1map
data NE3 a = a :*** [a]
  deriving (Functor, Foldable)

instance NFData a => NFData (NE3 a) where
    rnf (x :*** xs) = rnf x `seq` rnf xs

toNE3 :: NonEmpty a -> NE3  a
toNE3 (x :| xs) = x :*** xs

instance Foldable1 NE3 where
    foldr1map g f (x :*** xs) = go x xs where
        go y [] = g y
        go y (z : zs) = f (g y) (go z zs) 
