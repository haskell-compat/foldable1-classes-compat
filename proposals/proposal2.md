bAdd Foldable1 to base
=====================

Motivation
----------

It's regularly asked whether `Foldable1` could be added to `base`
(e.g. on reddit[^ref1], there're also GHC issue[^ref2] and old
phabricator diff[^ref3])
Also there's work towards non-empty maps and sets[^ref4],
which would benefit from `Foldable1`.
Recently `nonempty-vector` was upload to Hackage as well[^refV].

As commented on reddit, `Foldable1` could be added without any pain
to the `base` as it's pure addition - no modifications needed in
existing modules.

[^ref1]: https://www.reddit.com/r/haskell/comments/6d0vgt/could_we_have_foldable1_and_traversable1_in_base/
[^ref2]: https://gitlab.haskell.org/ghc/ghc/issues/13573
[^ref3]: https://phabricator.haskell.org/D4812
[^ref4]: https://github.com/haskell/containers/pull/616
[^refV]: https://hackage.haskell.org/package/nonempty-vector

Changelog
---------

- Remove `toNonEmpty` from MINIMAL pragma
- Add `Semifoldable` naming-scheme alternative (see sections at the end)
- Discuss `Bifoldable1`
- Discuss `foldr1` inefficiency
- Migration plan for `tagged` and `bifunctors`
- PoC patch to `semigroupoids`
- `foldable1` package has doctest examples, and a test-suite
- more members are manually implemented (and tested)


Change: Foldable1
-----------------

The change exist as merge request[^ghcMR] on gitlab.haskell.org.
However the more up to date version of a proposed module is visible from
haddocks on

    https://oleg.fi/haddocks/foldable1/Data-Foldable1.html

or

    http://oleg.fi/haddocks/semifoldable/Data-Semifoldable.html

Importantly, this change **doesn't change** anything in other modules
of `base`, except of adding a `Foldable` instance to `Data.Complex`.
In particular, `foldl1` and `foldr1` in `Data.Foldable` remain partial, etc.

My version of `Foldable1` class is big, so I'll comment the motivation
for each member

```haskell
class Foldable t => Foldable1 t where
    {-# MINIMAL foldMap1 | foldr1map #-}

    fold1 :: Semigroup m => t m -> m

    -- the defining member, like foldMap but only asking for Semigroup
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m

    -- strict foldMap1, cf foldMap'
    foldMap1' :: Semigroup m => (a -> m) -> t a -> m

    -- analogue of toList
    toNonEmpty :: t a -> NonEmpty a

    -- left&right, strict&non-strict folds
    foldr1  :: (a -> a -> a) -> t a -> a
    foldr1' :: (a -> a -> a) -> t a -> a
    foldl1  :: (a -> a -> a) -> t a -> a
    foldl1' :: (a -> a -> a) -> t a -> a

    -- these can have efficient implementation for NonEmptySet
    maximum1 :: forall a. Ord a => t a -> a
    minimum1 :: forall a. Ord a => t a -> a

    -- head1 have efficient implementation for NonEmpty and Tree
    -- last1 for symmetry
    head1 :: t a -> a
    last1 :: t a -> a

    -- fold variants with premap.
    -- Without this map, we cannot implement foldl using foldr etc.
    foldr1Map  :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldl1Map  :: (a -> b) -> (b -> b -> b) -> t a -> b
    foldr1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
```

The merge request also adds instances for everything non-empty in `base`.

I propose the `Data.Foldable1` as the module name (an alternative
`Data.Semifoldable`).
`semigroupoids`[^semigroupoids] uses `Data.Semigroup.Foldable`,
but it's confusing; and using different name could help migration.

Additionally, the `Data.Foldable1` module contains five top-level functions,
which should be self-explanatory:

```haskell
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m

foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a

maximum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimum1By :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
```

This is less than in `Data.Semigroup.Foldable`[^d.s.foldable], as other
top-level definitions doesn't make sense without bringing in the `Apply`
type-class.  For example:

```haskell
-- needs Apply, not in Data.Foldable1
traverse1_ :: (Foldable1 t, Apply f) => (a -> f b) -> t a -> f ()
```

And if we relax `Apply` to `Applicative`, we get `traverse_`.
Bringing `Apply` into `base` is out-of-scope of this proposal.

[^ghcMR]: https://gitlab.haskell.org/ghc/ghc/merge_requests/1973
[^semigroupoids]: https://hackage.haskell.org/package/semigroupoids
[^d.s.foldable]: https://hackage.haskell.org/package/semigroupoids-5.3.3/docs/Data-Semigroup-Foldable-Class.html

Bifoldable1
-----------

`Bifoldable` class have `Bifoldable1` subclass in `semigroupoids`.
We could move that class into `base` as well, but it's not strictly necessary,
as it can be done later too.

However, `Bifoldable1` should migrate to `bifunctors` package.
This is discussed in "Compatibility & migration" section.

Name controversy
----------------

Adding `Foldable1` is considered controversial.
Library submissions guidelines say:

> Adding a new, useful function under a clear name is probably not controversial

Yet in this case, there doesn't seem to be clear names.
The alternative naming scheme is discussed on `semigroupoids` issue
tracker[^naming-issue].

In a comment nickname chessai list a table of possible renamings,
essentially dropping `1`-suffix and adding `semi`- prefix.[^refComment1]
Following comments brainstorm more ideas like:

- all the functions that aren't actual typeclass methods could possibly just
  keep the `1` suffix
- i'm struggling against consistency here, because some functions sound great
  with `semi`- as their prefix, and some sound bad

The bad sounding names are `semihead`, `semilast`, `semimaximum` and
`semiminimum`. In theory they could be prefixless and suffixless,
i.e. plain `head`, `last`, `maximum`, and `minimum`. However,
I consider that naming more controversial, as it clashes with `Prelude`
names, even one can argue that `Semifoldable` members should
eventually replace them. Luckily, the names can be changed,
if they are on the way into `Prelude`.

A variation of this, is to use bare `s` as prefix to the members, i.e.
`sfoldMap`, `sfoldr`. It's shorter, but maybe too subtle?

One justification to not use 1-suffix name is[^refComment2]

> The 1 is still in conflict, but requires more Eq1, etc like classes to
> define. e.g. Monoid1 for taking a monoid to a monoid, then Foldable1
> consistently in that nomenclature would let you reduce through a Monoid1.

Also using qualified imports would prevent `Foldable1` class to be ever
imported unqualified[^refComment3]:

> The haddocks for Semi.Monad being a superclass of Monad someday in the far
> flung future would be frankly pretty awful to read, and would ensure that
> they could never move into Prelude, forever dooming them to a second class
> existence.

And finally, trying to unify `Foldable` with `Foldable1` into single class
using type families / some hackery requires `QuantifiedConstraints` at the
very least. That's not a realistic option to current, essentially a Haskell98
formulation.

[^naming-issue]: https://github.com/ekmett/semigroupoids/issues/26
[^refComment1]: https://github.com/ekmett/semigroupoids/issues/26#issuecomment-395565772
[^refComment2]: https://github.com/ekmett/semigroupoids/issues/26#issuecomment-395950042
[^refComment3]: https://github.com/ekmett/semigroupoids/issues/26#issuecomment-398117218

Inefficiency of foldr1
----------------------

In another `semigroupoids` issue[^foldr1-issue],
the inefficiency of `foldr1` is highlighted.

My original proposal included functions of the type:

```haskell
foldr1Map :: (a -> b) -> (b -> b -> b) -> t a -> b
```

Yet, Andrew Martin points out, another better type:

```haskell
foldr1Map :: (a -> b) -> (a -> b -> b) -> t a -> b
```

This helps differentiate between foldr and foldl variants,
and also simplifies some implementation bits (to my surprise).
I'm in favour of this change.

The order of function arguments is chosen so:

```haskell
foldr1 = foldr1Map id
```

This variant is implemented in a PR in my repository[^foldrPR].
But not yet incorporated into this proposal.

[^foldr1-issue]: https://github.com/ekmett/semigroupoids/issues/77
[^foldrPR]: https://github.com/phadej/foldable1/pull/7

Compatibility & migration
-------------------------

I drafted a compatibility package `foldable1`:

- GitHub repository:    https://github.com/phadej/foldable1
- haddocks:             https://oleg.fi/haddocks/foldable1/
- Semifoldable variant: https://github.com/phadej/foldable1/pull/5
- its haddocks:         https://oleg.fi/haddocks/semifoldable/

which I hope could be maintained under github.com/haskell organization.
I can act as a maintainer, with a hope that there won't be a lot
of changes happening in `Data.Foldable1`.

To my surprise, there's already a package with this name on
Hackage[^hackageFoldable] by
M Farkas-Dyck (cc'd). He kindly offered to donate the name if
this proposal is accepted (with foldable1 name).[^refDonate]

`Data.Foldable1` contains also instances for `Lift`, `Backwards` and `Reverse`,
and other data types from `transformers`. Perfectly, the `transformers` bundled
with GHC with this change would implement the instances as well.  This change
should propage to `transformers-compat` too.

Similarly, `containers` would have an instance for `Tree` (and non-empty
`Set` and `Map` when they are added).


Other packages would be compat'd as follows:
- `foldable1` would provide instances for `Tagged` from `tagged`
- `Bifoldable1` class would migrate to `bifunctors`

This is because current dependencies are:

```
semigroups <- tagged <- bifunctors <- semigroupoids
```

and `foldable1` would be more natural between `tagged` and `bifunctors`:

```
semigroups <- tagged <- foldable1 <- bifunctors <- semigroupoids
```

`foldable` have to be before `bifunctors` in the dependency tree,
as `Bifoldable1` instances of some `Bifunctor`s need `Foldable1` class.

I also drafted a PR for compatibility patch to `semigroupoids`[^semigroupoidsPatch]
including `Foldable1` part; but doesn't include 
migrating `Bifoldable, nor other proposed renaming.

The rest of renamings is straight-forward should be straight-forward to do.
Migration `Bifoldable` would be a lot easier, when the `foldable1` package
interface is stabilized.

[^hackageFoldable]: https://hackage.haskell.org/package/foldable1
[^refDonate]: https://mail.haskell.org/pipermail/libraries/2019-October/030029.html
[^semigroupoidsPatch]: https://github.com/ekmett/semigroupoids/pull/87

Unresolved questions
--------------------

- The names? Foldable1 or Semifoldable, members?
    - Bifoldable1 or Bisemifoldable (or Semibifoldable)?
    - Members: `semifoldMap` or just `sfoldMap`?
  See following Foldable1 and Semifoldable sections for synopsis
- Which type signature `foldr1Map` / `semifoldr1Map` should have (`a -> b -> b` is IMO better)
- GHC-8.10 freeze is quite soon, is targeting GHC-8.12/base-4.15 more realistic.
  Note: this technically is a non-breaking change in `base`,
  so could be bundled with GHC-8.10.2, but I think sticking to major would be
  preferable by GHC HQ.

Appendix: Foldable1 synopsis
----------------------------

https://oleg.fi/haddocks/foldable1/Data-Foldable1.html

```haskell
class Foldable t => Foldable1 t where
  fold1      :: Semigroup m => t m -> m
  foldMap1   :: Semigroup m => (a -> m) -> t a -> m
  foldMap1'  :: Semigroup m => (a -> m) -> t a -> m

  foldr1     :: (a -> a -> a) -> t a -> a
  foldr1'    :: (a -> a -> a) -> t a -> a
  foldl1     :: (a -> a -> a) -> t a -> a
  foldl1'    :: (a -> a -> a) -> t a -> a

  toNonEmpty :: t a -> NonEmpty a

  maximum1   :: forall a. Ord a => t a -> a
  minimum1   :: forall a. Ord a => t a -> a
  head1      :: t a -> a
  last1      :: t a -> a

  foldr1Map  :: (a -> b) -> (b -> b -> b) -> t a -> b
  foldl1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
  foldl1Map  :: (a -> b) -> (b -> b -> b) -> t a -> b
  foldr1'Map :: (a -> b) -> (b -> b -> b) -> t a -> b

intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
foldrM1      :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1      :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
maximum1By   :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimum1By   :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
```

Appendix: Semifoldable synopsis
-------------------------------

https://oleg.fi/haddocks/semifoldable/

```haskell
class Foldable t => Semifoldable t where
  semifold      :: Semigroup m => t m -> m
  semifoldMap   :: Semigroup m => (a -> m) -> t a -> m
  semifoldMap'  :: Semigroup m => (a -> m) -> t a -> m

  semifoldr     :: (a -> a -> a) -> t a -> a
  semifoldr'    :: (a -> a -> a) -> t a -> a
  semifoldl     :: (a -> a -> a) -> t a -> a
  semifoldl'    :: (a -> a -> a) -> t a -> a

  toNonEmpty    :: t a -> NonEmpty a

  semimaximum   :: forall a. Ord a => t a -> a
  semiminimum   :: forall a. Ord a => t a -> a
  semihead      :: t a -> a
  semilast      :: t a -> a

  semifoldrMap  :: (a -> b) -> (b -> b -> b) -> t a -> b
  semifoldl'Map :: (a -> b) -> (b -> b -> b) -> t a -> b
  semifoldlMap  :: (a -> b) -> (b -> b -> b) -> t a -> b
  semifoldr'Map :: (a -> b) -> (b -> b -> b) -> t a -> b

intercalate1  :: (Semifoldable t, Semigroup m) => m -> t m -> m
foldrM1       :: (Semifoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1       :: (Semifoldable t, Monad m) => (a -> a -> m a) -> t a -> m a
semimaximumBy :: Semifoldable t => (a -> a -> Ordering) -> t a -> a
semiminimumBy :: Semifoldable t => (a -> a -> Ordering) -> t a -> a

-- or alternatively
semiintercalate
semifoldrM
semifoldlM
```

Appendix: Alternative foldr1Map
-------------------------------

```haskell
class Foldable t => Foldable1 t where
  fold1      :: Semigroup m => t m -> m
  foldMap1   :: Semigroup m => (a -> m) -> t a -> m
  foldMap1'  :: Semigroup m => (a -> m) -> t a -> m

  foldr1     :: (a -> a -> a) -> t a -> a
  foldr1'    :: (a -> a -> a) -> t a -> a
  foldl1     :: (a -> a -> a) -> t a -> a
  foldl1'    :: (a -> a -> a) -> t a -> a

  toNonEmpty :: t a -> NonEmpty a

  maximum1   :: forall a. Ord a => t a -> a
  minimum1   :: forall a. Ord a => t a -> a
  head1      :: t a -> a
  last1      :: t a -> a

  -- These four are changed compared to Foldable1 synopsis
  foldr1Map  :: (a -> b) -> (a -> b -> b) -> t a -> b
  foldl1'Map :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldl1Map  :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldr1'Map :: (a -> b) -> (a -> b -> b) -> t a -> b

intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
foldrM1      :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1      :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
maximum1By   :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimum1By   :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
```
