-- | Surgery for generic data types:
-- remove and insert constructors and fields.
--
-- Functions in this module are expected to be used with visible type
-- applications. They have a lot of type parameters, but only the first two or
-- three usually matter. Functions are annotated with \"functional
-- dependencies\", with a meaning similar to the homonymous GHC extension for
-- type classes (click on \"Details\" under each function to see those).
--
-- Note that not all parameters to the left of a functional dependency arrow
-- need to be annotated explicitly to determine those on the right. Some can
-- also be inferred from the context.

module Generic.Data.Surgery
  ( -- * Getting into the operating room
    OR

  , toOR
  , toData
  , fromData
  , fromOR

  , OROf

    -- * Surgeries
  , removeCField
  , insertCField
  , removeRField
  , insertRField
  , removeConstr
  , insertConstr

    -- * Constraint synonyms

    -- | Hiding implementation details from the signatures above.
    -- Useful to compose surgeries in a reusable way.

    -- ** Conversions

  , ToORRep
  , ToOR
  , FromORRep
  , FromOR

    -- ** Surgeries

  , RmvCField
  , InsCField
  , RmvRField
  , InsRField
  , RmvConstr
  , InsConstr
  ) where

import Generic.Data.Internal.Surgery
