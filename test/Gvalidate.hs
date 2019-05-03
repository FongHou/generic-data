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

class (a ~ f b) => F f a b
instance (a ~ f b) => F f a b

gvalidate
  :: forall t t' f
   . (ADT t t', Constraints t t' (F f), Applicative f)
  => t
  -> f t'
gvalidate t = gtraverse @(F f) id t

