# ghc-source-gen
TEST

`ghc-source-gen` is a Haskell library for constructing Haskell syntax trees using the GHC API.  This package is compatible with multiple versions of GHC: currently, 8.2, 8.4, 8.6, and 8.8.

This is not an officially supported Google product.

## Example

This example constructs and prints a module defining the
`const` function:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import GHC.SourceGen
import GHC.Paths (libdir)

constModule :: HsModule'
constModule =
    module' (Just "Const") (Just [var "const"]) []
        [ typeSig "const" $ a --> b --> a
        , funBind "const" $ matchRhs [wildP, x] x
        ]
  where
    a = var "a"
    b = var "b"
    x = var "x"

main = runGhc (Just libdir) $ putPpr constModule
```

Which will output:

```
module Const (
        const
    ) where
const :: a -> b -> a
const _ x = x
```

## Syntax Types

GHC represents Haskell syntax trees with several parametrized datatypes; for example: `HsExpr p` for expressions, `HsDecl p` for declarations, etc.  The parameter `p` determines which stage of compilation that data has last completed: parsing, renaming, or type-checking.

`ghc-source-gen` constructs values as GHC would represent them
immediately after the parsing step.  In ghc-8.6, that
corresponds to `p` being `GhcPs`.  It defines several type
synonyms, such as:

```haskell
type HsExpr' = HsExpr GhcPs
type HsType' = HsType GhcPs
type HsDecl' = HsDecl GhcPs
type HsModule' = HsModule GhcPs
-- etc.
```

GHC's datatypes generally contain location information in the
form of [`SrcSpan`](http://hackage.haskell.org/package/ghc/docs/SrcLoc.html#t:SrcSpan) values which point to their original
location in a source file.  `ghc-source-gen` constructs values
at runtime, so it uses a dummy value for `SrcSpan` on using a
dummy SrcSpan.  (GHC does something similar for code written at the interactive GHCi prompt.)

`ghc-source-gen` aims to be a low-level wrapper around GHC's
types.  In particular, it does not explicitly help the user
generate unique names like, for example, `template-haskell`'s
[`newName`](http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:newName)
action.  However, we may add support for that in future
versions of this library.
