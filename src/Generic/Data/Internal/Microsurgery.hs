{-# LANGUAGE
    AllowAmbiguousTypes,
    DataKinds,
    FlexibleContexts,
    PolyKinds,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Generic.Data.Internal.Microsurgery where

import Data.Coerce (Coercible, coerce)
import GHC.Generics
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import Generic.Data.Types

-- * Derecordify

derecordify ::
  Coercible (Derecordify f) f =>
  -- Coercible is not symmetric!??
  Data f p -> Data (Derecordify f) p
derecordify = coerce

underecordify ::
  Coercible f (Derecordify f) =>
  Data (Derecordify f) p -> Data f p
underecordify = coerce

-- | Forget that a type was declared using record syntax.
--
-- @
-- data Foo = Bar { baz :: Zap }
--
-- -- becomes --
--
-- data Foo = Bar Zap
-- @
--
-- Concretely, set the last field of 'MetaCons' to 'False' and forget field
-- names.
type family Derecordify (f :: k -> *) :: k -> *
type instance Derecordify (M1 D m f) = M1 D m (Derecordify f)
type instance Derecordify (f :+: g) = Derecordify f :+: Derecordify g
type instance Derecordify (f :*: g) = Derecordify f :*: Derecordify g
type instance Derecordify (M1 C ('MetaCons nm fx _isRecord) f) = M1 C ('MetaCons nm fx 'False) (Derecordify f)
type instance Derecordify (M1 S ('MetaSel _nm su ss ds) f) = M1 S ('MetaSel 'Nothing su ss ds) f
type instance Derecordify V1 = V1
type instance Derecordify U1 = U1

-- * Type aging ("denewtypify")

typeage ::
  Coercible (Typeage f) f =>
  Data f p -> Data (Typeage f) p
typeage = coerce

untypeage ::
  Coercible f (Typeage f) =>
  Data (Typeage f) p -> Data f p
untypeage = coerce

-- | Forget that a type is a @newtype@.
--
-- @
-- newtype Foo = Bar Baz
--
-- -- becomes --
--
-- data Foo = Bar Baz
-- @
type family Typeage (f :: k -> *) :: k -> *
type instance Typeage (M1 D ('MetaData nm md pk _nt) f) = M1 D ('MetaData nm md pk 'False) f

-- * Renaming

renameFields ::
  forall rnm f p.
  Coercible (RenameFields rnm f) f =>
  Data f p -> Data (RenameFields rnm f) p
renameFields = coerce

unrenameFields ::
  forall rnm f p.
  Coercible (RenameFields rnm f) f =>
  Data f p -> Data (RenameFields rnm f) p
unrenameFields = coerce

renameConstrs ::
  forall rnm f p.
  Coercible (RenameConstrs rnm f) f =>
  Data f p -> Data (RenameConstrs rnm f) p
renameConstrs = coerce

unrenameConstrs ::
  forall rnm f p.
  Coercible (RenameConstrs rnm f) f =>
  Data f p -> Data (RenameConstrs rnm f) p
unrenameConstrs = coerce

-- | @f \@\@ s@ is the application of a type-level function symbolized by @f@
-- to a @s :: 'Symbol'@.
--
-- A function @FooToBar@ can be defined as follows:
--
-- @
-- data FooToBar
-- type instance FooToBar '@@' \"foo\" = \"bar\"
-- @
type family (f :: *) @@ (s :: Symbol) :: Symbol

-- | Identity function @'Symbol' -> 'Symbol'@.
data SId
type instance SId @@ s = s

-- | Empty function (compile-time error when applied).
data SError
type instance SError @@ s = TypeError ('Text "Invalid name: " ':<>: 'ShowType s)

-- | Constant function.
data SConst (s :: Symbol)
type instance SConst z @@ _s = z

-- | Define a function for a fixed set of strings, and fall back to @f@ for the others.
data SRename (xs :: [(Symbol, Symbol)]) (f :: *)
type instance SRename xs f @@ s = SRename' xs f s

-- | Closed type family for 'SRename'.
type family SRename' (xs :: [(Symbol, Symbol)]) (f :: *) (s :: Symbol) where
  SRename' '[] f s = f @@ s
  SRename' ('( s,  t) ': _xs) _f s = t
  SRename' ('(_r, _t) ':  xs)  f s = SRename' xs f s

-- | Rename fields using the function @rnm@ given as a parameter.
type family RenameFields (rnm :: *) (f :: k -> *) :: k -> *
type instance RenameFields rnm (M1 D m f) = M1 D m (RenameFields rnm f)
type instance RenameFields rnm (f :+: g) = RenameFields rnm f :+: RenameFields rnm g
type instance RenameFields rnm (f :*: g) = RenameFields rnm f :*: RenameFields rnm g
type instance RenameFields rnm (M1 C m f) = M1 C m (RenameFields rnm f)
type instance RenameFields rnm (M1 S ('MetaSel ('Just nm) su ss ds) f) = M1 S ('MetaSel ('Just (rnm @@ nm)) su ss ds) f

-- | Rename constructors using the function @rnm@ given as a parameter.
type family RenameConstrs (rnm :: *) (f :: k -> *) :: k -> *
type instance RenameConstrs rnm (M1 D m f) = M1 D m (RenameConstrs rnm f)
type instance RenameConstrs rnm (f :+: g) = RenameConstrs rnm f :+: RenameConstrs rnm g
type instance RenameConstrs rnm (f :*: g) = RenameConstrs rnm f :*: RenameConstrs rnm g
type instance RenameConstrs rnm (M1 C ('MetaCons nm fi ir) f) = M1 C ('MetaCons (rnm @@ nm) fi ir) f
