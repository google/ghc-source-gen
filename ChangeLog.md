# Changelog for haskell-syntax

## 0.1.1.0
- Improved comments for instances of `HasValBind'.
- Generalized `patBind` to any `HasValBind` so it can be used
  with `let'` and `while'`.  The old version is still exported
  from `GHC.SourceGen.Decl`, but deprecated.  The new version is
  exported from `GHC.SourceGen.Binds` and `GHC.SourceGen`.

## 0.1.0.0
Initial version.
