-- | Simple operations on generic representations.
--
-- More complex ones can be found in
-- <generic-data-surgery https://hackage.haskell.org/package/generic-data-surgery>.

module Generic.Data.Microsurgery
  ( -- * Synthetic types
    -- | See also "Generic.Data.Types"

    Data
  , toData
  , fromData

    -- * Microsurgeries

  , unrecordify
  , recordify
  , Unrecordify
  ) where

import Generic.Data.Internal.Data
import Generic.Data.Internal.Microsurgery
