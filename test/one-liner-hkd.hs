{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor.Identity
import Generics.OneLiner
import qualified Generics.OneLiner.Binary as G2
import GHC.Generics
import Relude.Extra.Validation

-- gvalidate implementation
class (a ~ f b) => Is f a b

instance (a ~ f b) => Is f a b

gvalidate :: forall t t' f.
          (G2.ADT t t',G2.Constraints t t' (Is f),Applicative f)
          => t
          -> f t'
gvalidate t = G2.gtraverse @(Is f) id t

-- helpers for gmemptyA and gmemptyA
class (a ~ f (UnWrap a)) => Wrap f a

instance (a ~ f (UnWrap a)) => Wrap f a

type family UnWrap (a :: *) :: * where
  UnWrap (f b) = b

gmemptyA :: forall t f. 
         (ADTRecord t,Constraints t (Wrap f), Alternative f) 
         => t
gmemptyA = nullaryOp @(Wrap f) empty

gmappendA :: forall t f.
          (ADTRecord t,Constraints t (Wrap f), Alternative f)
           => t -> t -> t
gmappendA = binaryOp @(Wrap f) (<|>)

-- HKD
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- Example
data Person' f =
  Person {name :: HKD f String
         ,age :: HKD f Int}
  deriving Generic

deriving instance (Constraints (Person' f) Show) => Show (Person' f)

type Person = Person' Identity

type PersonMaybe = Person' Maybe

type PersonError = Person' (Validation [String])

main :: IO ()
main =
  do let pm1 :: PersonMaybe = Person (Just "Bob") (Just 16)
     let mp1 :: Maybe Person = gvalidate $ gmappendA gmemptyA pm1
     print mp1
     let pe1 :: PersonError = Person (Success "Bob") (Success 10)
     let ep1 :: PersonError = gmappendA gmemptyA pe1
     print $ validate ep1 
     return ()
