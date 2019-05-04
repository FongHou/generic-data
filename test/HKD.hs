{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

import Control.Applicative
import Control.Monad.Trans.Identity
import Data.Aeson hiding (Success)
import Data.Coerce
import Data.Either.Validation
import Data.Functor.Const
import Data.Functor.Identity
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Monoid hiding (First (..), Last (..))
import Data.Semigroup
import Generic.Data
import Generic.Data.Orphans ()
import Generics.OneLiner
import GHC.Generics
import Lens.Micro hiding (to, from)

import Gvalidate

gcoerce 
  :: forall a b . (Coercible (Rep a) (Rep b), Generic a, Generic b)
  => a -> b
gcoerce = to . coerce' . from
 where
  coerce' :: Coercible f g => f () -> g ()
  coerce' = coerce

gcoerceBinop
  :: forall a b . (Coercible (Rep a) (Rep b), Generic a, Generic b)
  => (a -> a -> a)
  -> (b -> b -> b)
gcoerceBinop binop x y = gcoerce (gcoerce x `binop` gcoerce y)

-- type family HKD f a where
--   HKD Identity a = a
--   HKD f a = f a

type family HKD f a where
  HKD Identity a = a
  HKD (Const u) a = u
  HKD (IdentityT f) a = f a
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

data Animal = Animal
  { pName :: String
  , pAge  :: Int
  }
  deriving (Show, Generic)

a1 = Animal "Puppy" 1

p0 :: Person
p0 = gcoerce a1

data Person' f = Person
  { pName :: HKD f String
  , pAge  :: HKD f Int
  }
  deriving (Generic)

type Person = Person' Identity
type LastPerson = Person' Last

newtype SSN = SSN String
  deriving (Show, Generic, ToJSON, FromJSON)

data Employee' f = Employee
  { pName :: HKD f String
  , pAge  :: HKD f Int
  , pSsn  :: HKD f SSN
  } deriving (Generic)

type Employee = Employee' Identity
type MaybeEmployee = Employee' Maybe
type MaybeEmployee1 = Employee' (IdentityT Maybe)
type ValidEmployee = Employee' (IdentityT (Validation String))

deriving instance (Constraints (Person' f) Show) => Show (Person' f)
deriving instance (Constraints (Employee' f) Show) => Show (Employee' f)

-- instance Semigroup LastPerson where
--   (<>) = gmappend
instance Semigroup (Rep (Person' f) ()) => Semigroup (Person' f) where
  (<>) = gmappend

instance (Semigroup u) => Semigroup (Employee' (Const u)) where
  (<>) = gmappend

instance Alternative f => Semigroup (Employee' (IdentityT f)) where
  (<>) = gcoerceBinop (gmappend @(Employee' (IdentityT (Alt f))))

instance Alternative f => Monoid (Employee' (IdentityT f)) where
  mempty = gcoerce (gmempty @(Employee' (IdentityT (Alt f))))
  mappend = (<>)

instance ToJSON Person
instance FromJSON Person
instance ToJSON LastPerson
instance FromJSON LastPerson

deriving instance (Constraints (Employee' f) ToJSON) => ToJSON (Employee' f)
deriving instance (Constraints (Employee' f) FromJSON) => FromJSON (Employee' f)

p1, p2 :: LastPerson
p1 = Person { pName = Last "Bob", pAge = Last 10 }
p2 = Person { pName = Last "Bob", pAge = Last 20 }

-- validate infers type!
-- p1', p2' :: Last Person
p1' = getLast $ validate p1
p2' = getLast $ validate p2

-- gvalidate needs type!
p12 :: Person
p12 = getLast $ gvalidate $ p1 <> p2

e0 :: MaybeEmployee
e0 = Employee { pName = Nothing, pAge = Just 18, pSsn = Nothing }

e1 :: MaybeEmployee
e1 = Employee { pName = Just "Joe", pAge = Just 23, pSsn = Just (SSN "123-4567-890") }

_ = validate e0
_ = validate e1


-- lift f ~ (IdentityT f) to get Alternative instance
e0', e1', e01 :: MaybeEmployee1
e0' = gcoerce e0
e1' = gcoerce e1
e01 = mempty <> e1' <> e0' <> mempty

e2 :: ValidEmployee
e2 = Employee { pName = Success "Joe"
              , pAge  = Success 40
              , pSsn  = Failure "Employee missing SSN;"
              }

e3 :: ValidEmployee
e3 = Employee { pName = Failure "Employee missing Name;"
              , pAge  = Failure "Employee missing Age;"
              , pSsn  = Success (SSN "123-4567-890")
              }

e2', e3' :: Validation _ Employee
e2' = gvalidate e2
e3' = gvalidate e3

main :: IO ()
main = do
  let Just (e01' :: Employee) = gvalidate e01
  print $ encode e01'
  print e2'
  print e3'
  let Success e32 = gvalidate $ e3 <> e2
  print $ encode e32
  print $ e32 ^. #pName
  -- doesn't work!? 
  -- print $ set #pAge 11 e32
  -- why set #label choose HasField instance, not HasField'?
  -- print $ set (field @"pAge") 11 e32
  print $ set (field' @"pAge") 11 e32
  let age :: Lens' Employee Int
      age = #pAge
  print $ set age 10 e32
  print $ e32 ^. age
