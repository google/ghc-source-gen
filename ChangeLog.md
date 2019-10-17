# Changelog for haskell-syntax

## 0.3.0.0
- Add `occNameToStr` and `nameToStr` to convert from the GHC types.

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
