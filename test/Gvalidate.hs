{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Gvalidate
where

import Generics.OneLiner.Binary
import Data.Functor.Identity

class (a ~ f b) => F f a b
instance (a ~ f b) => F f a b

gvalidate
  :: forall t t' f . (ADT t t', Constraints t t' (F f), Applicative f)
  => t -> f t'
gvalidate = gtraverse @(F f) id

-- is there a way to lift pure fields (no Identity wrapper) into applictives 
-- using pure :: (Applicative f) => a -> f a
-- gpure :: ...  => t -> t' f
-- e.g.:  Person -> Person' Mabye

-- are there higher kinded generic fmap and liftA2?
-- gfmap :: ... => (f ~> g) -> t f -> t g
-- e.g. :  t Maybe ~> t (Validation [b])
--
-- gliftA2 :: ... => (f ~> g ~>) -> t f -> t g -> t h
-- e.g.: t (Const b) ~> t Maybe ~> t (Validation [b])

-- hence, Higher Kinded Applicative!
