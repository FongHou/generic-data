-- | Generic combinators to derive type class instances.

module Generic.Data
  ( -- * Deriving instances

    -- | Classes that GHC can not derive (excluding @GeneralizedNewtypeDeriving@):
    -- 'Data.Semigroup.Semigroup', 'Monoid', 'Applicative', and
    -- 'Control.Applicative.Alternative'.
    --
    -- On base < 4.11 (i.e., GHC < 8.6), you must import "Generic.Data.Orphans"
    -- for generic deriving of 'Data.Semigroup.Semigroup' and 'Monoid'.

    -- ** 'Data.Semigroup.Semigroup'
    gmappend

    -- ** 'Monoid'
  , gmempty

    -- ** 'Applicative'
  , gpure
  , gap
  , gliftA2

    -- ** 'Control.Applicative.Alternative'
  , gempty
  , galt

    -- ** 'Eq'
  , geq

    -- ** 'Ord'
  , gcompare

    -- ** 'Show'
  , gshowsPrec
  , GShow()

    -- ** 'Enum'
  , gfromEnum
  , gtoEnum
  , GEnum()

    -- ** 'Bounded'
  , gminBound
  , gmaxBound
  , GBounded()

    -- ** 'Functor'
  , gfmap
  , gconstmap

    -- ** 'Foldable'
  , gfoldMap
  , gfoldr

    -- ** 'Traversable'
  , gtraverse
  , gsequenceA

    -- * Accessing metadata
  , typeName
  ) where

import Generic.Data.Prelude
import Generic.Data.Enum
import Generic.Data.Meta
import Generic.Data.Show
