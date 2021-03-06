# 0.6.0.1

- Fix derivation of `Show1` for `(:.:)`

# 0.6.0.0

- Add `Surgery` newtype for DerivingVia
- `Derecordify`, `Typeage`, `RenameFields`, `RenameConstrs`, `OnFields`
  are no longer type families, but defunctionalized symbols
  to be applied using `GSurgery`.

# 0.5.0.0

- Specialize `onData` to `Data`
- Add some instances for `U1` and `V1` in `Microsurgery`
- Add `OnFields` and `DOnFields` surgeries ("higher-kindification")

# 0.4.0.0

- Created `Microsurgery` module. Initial set of surgeries:

    + `Derecordify`
    + `Typeage`
    + `RenameFields`, `RenameConstrs`
    + Some doc about using generic-lens for surgeries

# 0.3.0.0

- Add generic implementations of `enumFrom`, `enumFromThen`, `enumFromTo`,
  `enumFromThenTo`. They are actually required to be explicit for correct
  `Enum` instances. Thanks to Topsii.
- Parameterize `GEnum` by a type-level option, and add `FiniteEnum` option
  to allow `Enum` to be derived for composite types. Thanks to Topsii.

# 0.2.0.0

- Remove `Generic.Data.Types.Map`
- Add `Generic.Data.Data.Types.toData` and `fromData`
- Remove `Defun` module (subsumed by `first-class-families` package)

# 0.1.1.0

- Add `gconIndex`
- Interface for constructor tags
- Type-level `Meta` accessors
- Add basic `Newtype` functions

# 0.1.0.0

Released generic-data
