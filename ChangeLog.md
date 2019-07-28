# Changelog for haskell-syntax

## 0.2
- Improved comments for instances of `HasValBind'.
- Generalized `patBind` to any `HasValBind` so it can be used
  with `let'` and `while'`.
- Remove `GHC.SourceGen.Syntax` and export types from relevant modules
  (for example, `HsType'` from `GHC.SourceGen.Type`).

## 0.1.0.0
Initial version.
