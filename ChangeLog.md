# Changelog for haskell-syntax


# 0.4.6.1
- Remove support for GHC < 9.0.

# 0.4.6.0
- Support GHC 9.10.

# 0.4.5.0
- Support GHC 9.8.

# 0.4.4.1
- Repair unintentionally broken GHC 8.10.7 support.

# 0.4.4.0
- Support GHC 9.6.

# 0.4.3.0
- Support GHC 9.2.

# 0.4.2.0
- Support GHC 9.

# 0.4.1.0
- Add `funBindsWithFixity`.
- Add `standaloneDeriving`.

# 0.4.0.0

## Breaking Changes
- Functions defining types and classes now take their
  type parameters as `HsTyVarBndr'` rather than `OccNameStr`.
  To construct a `HsTyVarBndr'`, use either `bvar` or `kindedVar`.
  Affects: `class'`, `type'`, `newtype'`, and `data'`.
- Remove support for ghc-8.2.*.

## Other Changes
- Add support for ghc-8.10.
- Add `kindedVar`.
- Add `tuplePromotedTy`.
- Add `from`, `fromTo`, `fromThen`, `fromThenTo`.
- Add `listComp`.

## 0.3.0.0
- Add `occNameToStr` and `nameToStr` to convert from the GHC types.
- Make `listPromotedTy` emit the promoted form `'[..]`,
  to distinguish from regular list types of zero or one elements.

## 0.2.0.1
- Bump upper-bound to allow `QuickCheck-2.13`.

## 0.2
- Improve overall documentation..
- Move `patBind` to a new `HasPatBind` class so that it can be used
  with `let'` and `while'`.
- Remove `GHC.SourceGen.Syntax` and export types from relevant modules
  (for example, `HsType'` from `GHC.SourceGen.Type`).
- Refactor the treatment of names:
    - Rename the constructors of `RdrNameStr`.
    - Make some combinators take `OccNameStr` instead of `RdrNameStr`, and
      introduce the `BVar` class for patterns.
    - Add `Eq` and `Ord` instances.
- Support more kinds of syntax:
    - Deriving clauses
    - Import/export lists
    - Record expressions and patterns
    - Type family instances
    - Pattern synonyms (currently, only the prefix form)
- Refactor the names and types of `match` and related combinators,
  optimizing for the common case of a single expression on the RHS.
- Fix edge cases when parsing qualified operators.
- Add parentheses in some cases that were previously missing them.
- Make `==>` have the same precedence as `-->` to make it easier to
  combine them.
- Make `tyApp`'s precedence match `-XTypeApplications`.
- Fix pretty-printing of rational literals.
- Make `tyPromotedVar` pretty-print as `'Abc` not `Abc`.
- Add `conP_` for pattern constructors with no arguments.

## 0.1.0.0
Initial version.
