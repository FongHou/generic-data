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

-- Can pure fields be lifted into applictives using pure :: (Applicative f) => a -> f a
-- e.g. Person -> Person' Mabye
-- gpure
--   :: ...
--   => t -> t f

-- Is there a generic FFunctor ffmap?
-- e.g. Person' Maybe ~> Person' Validation [String] a
-- gffmap
--   :: ...
--   => (forall x. g x -> h x)
--   ->  f g -> f h
