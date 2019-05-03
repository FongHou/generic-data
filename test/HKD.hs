{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module HKD where

import Control.Applicative
import Control.Monad.Trans.Identity
import Data.Aeson hiding (Success)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Monoid hiding (First(..), Last(..))
import Data.Semigroup
import Data.Coerce
import Data.Validation hiding (validate)
import Generic.Data hiding (gtraverse)
import Generic.Data.Orphans ()
import Generics.OneLiner
import GHC.Generics
import Prelude hiding (First (..), Last (..))

import Gvalidate

gcoerce
  :: forall a b . (Coercible (Rep a) (Rep b), Generic a, Generic b) => a -> b
gcoerce = to . coerce' . from
 where
  coerce' :: Coercible f g => f () -> g ()
  coerce' = coerce

gcoerceBinop
  :: forall a b
   . (Coercible (Rep a) (Rep b), Generic a, Generic b)
  => (a -> a -> a)
  -> (b -> b -> b)
gcoerceBinop binop x y = gcoerce (gcoerce x `binop` gcoerce y)

-- type family HKD f a where
--   HKD Identity a = a
--   HKD f a = f a

type family HKD f a where
  HKD Identity a = a
  HKD (IdentityT f) a = f a
  HKD (Const u) a = u
  HKD f a = f a

class GValidate f i o where
  gvalidate_ :: i p -> f (o p)

instance Functor f => GValidate f (K1 a (f k)) (K1 a k) where
  gvalidate_ (K1 k) = fmap K1 k

instance (Applicative f, GValidate f i o, GValidate f i' o')
  => GValidate f (i :*: i') (o :*: o') where
  gvalidate_ (l :*: r) = (:*:) <$> gvalidate_ l <*> gvalidate_ r

instance (Applicative f, GValidate f i o, GValidate f i' o')
  => GValidate f (i :+: i') (o :+: o') where
  gvalidate_ (L1 l) = L1 <$> gvalidate_ l
  gvalidate_ (R1 r) = R1 <$> gvalidate_ r

instance (Functor f, GValidate f i o)
  => GValidate f (M1 _a _b i) (M1 _a' _b' o) where
  gvalidate_ (M1 x) = M1 <$> gvalidate_ x

instance (Applicative f)
  => GValidate f U1 U1 where
  gvalidate_ U1 = pure U1

instance GValidate f V1 V1 where
  gvalidate_ = error "absurd"

validate
  :: ( Generic (hkd f)
     , Generic (hkd Identity)
     , GValidate f (Rep (hkd f)) (Rep (hkd Identity))
     , Applicative f
     )
  => hkd f
  -> f (hkd Identity)
validate = fmap to . gvalidate_ . from

--------------------------
data Person' f = Person
  { pName :: HKD f String
  , pAge  :: HKD f Int
  }
  deriving (Generic)

newtype SSN = SSN String
  deriving (Show, Generic, ToJSON, FromJSON)

data Employee' f = Employee
  { pName :: HKD f String
  , pAge  :: HKD f Int
  , pSsn  :: HKD f SSN
  } deriving (Generic)

type Person = Person' Identity
type LastPerson = Person' Last

type Employee = Employee' Identity
type MaybeEmployee = Employee' (IdentityT Maybe)
type ValidEmployee = Employee' (IdentityT (Validation [(Integer, String)]))

deriving instance (Constraints (Person' f) Show) => Show (Person' f)
deriving instance (Constraints (Employee' f) Show) => Show (Employee' f)

instance Semigroup (Rep (Person' f) ()) => Semigroup (Person' f) where
  (<>) = gmappend

instance (Semigroup u) => Semigroup (Employee' (Const u)) where
  (<>) = gmappend

instance Alternative f => Semigroup (Employee' (IdentityT f)) where
  (<>) = gcoerceBinop (gmappend @(Employee' (IdentityT (Alt f))))

-- instance Semigroup LastPerson where
--   (<>) = gmappend

instance ToJSON Person
instance FromJSON Person
instance ToJSON LastPerson
instance FromJSON LastPerson
instance FromJSON MaybeEmployee
instance ToJSON MaybeEmployee

-- deriving instance (Constraints (Employee' f) ToJSON) => ToJSON (Employee' f)
-- deriving instance (Constraints (Employee' f) FromJSON) => FromJSON (Employee' f)

-- p1, p2 :: LastPerson
p1 = Person { pName = Last "X", pAge = Last 10 }
p2 = Person { pName = Last "Y", pAge = Last 20 }

-- p1', p2' :: Last Person
p1' = validate p1
p2' = validate p2

p12 = getLast $ validate $ p1 <> p2

e0 :: MaybeEmployee
e0 = Employee { pName = Nothing, pAge = Just 18, pSsn = Nothing }

e1 :: MaybeEmployee
e1 = Employee { pName = Just "Joe"
              , pAge  = Just 23
              , pSsn  = Just (SSN "123-4567-890")
              }

e01 = e0 <> e1

e0', e1', e01' :: _ Employee
e0' = gvalidate e0
e1' = gvalidate e1
e01' = gvalidate e01

e2 :: ValidEmployee
e2 = Employee { pName = Success "Joe"
              , pAge  = Success 30
              , pSsn  = Failure [(3, "Employee missing SSN")]
              }

-- Success e2' = validate e2

e3 :: ValidEmployee
e3 = Employee { pName = Failure [(1, "Employee missing Name")]
              , pAge  = Success 40
              , pSsn  = Success (SSN "123-4567-890")
              }

e2', e3'  :: Validation _ Employee
e2' = gvalidate e2
e3' = gvalidate e3

-- This should work if Validation has an Alternative instance
-- e23, e32 :: ValidEmployee
-- e23 = e2 <> e3
-- e32 = e3 <> e2

