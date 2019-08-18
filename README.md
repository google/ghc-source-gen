# ghc-source-gen

`ghc-source-gen` is a Haskell library for generating Haskell source files and
code fragments.  It uses GHC's [library API] to support the latest syntax, and
provides a simple, consistent interface across several major versions of GHC.

To get started, take a look at the [example](#example) below, or check out the
`GHC.SourceGen` module.

This package is not an officially supported Google product.

[library API]: https://hackage.haskell.org/package/ghc

## Example

The following example creates a module that defines the
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
        , funBind "const" $ match [wildP, x] x
        ]
  where
    a = var "a"
    b = var "b"
    x = var "x"

main = runGhc (Just libdir) $ putPpr constModule
```

The output of that program is:

```
module Const (
        const
    ) where
const :: a -> b -> a
const _ x = x
```

## Comparison with the GHC API

The raw GHC API has several complexities that `ghc-source-gen` simplifies for the
purpose of source code generation.

### Backwards-compatibility
`ghc-source-gen` provides the same API across several versions of GHC.  Code written
with `ghc-source-gen` should compile unchanged on each of those versions.

Currently, this library supports GHC versions 8.2, 8.4, 8.6 and 8.8.

One caveat: `ghc-source-gen` supports some forms of syntax
which are not implemented by all of those GHC versions.  For example, the
`DerivingVia` extension is only implemented in `ghc >= 8.6`.  When built on
older versions of GHC, `ghc-source-gen` will omit functions for constructing
that syntax (for example: `GHC.SourceGen.Decl.derivingVia`).  We will also tag
any such function with a note in its Haddock documentation.

### Less verbose types and construction functions

The datatypes that GHC uses to represent Haskell syntax change their
representation at different stages of the compilation: for example, parsing,
renaming, or type-checking.  That data transformation provides type safety and
a uniform structure across the phases.  However, it also adds unnecessary
complexity to the task of source code generation.

`ghc-source-gen` aims to provide a simple interface by creating data types as
GHC would represent them immediately after its *parsing* step.  For example,
`ghc >= 8.4` uses a type parameter `p` in its syntax types: `HsExpr p` for
expressions, `HsDecl p` for declarations, etc.  `ghc-source-gen` defines type
synonyms for them:

```haskell
type HsExpr' = HsExpr GhcPs
type HsDecl' = HsDecl GhcPs
type HsType' = HsType GhcPs
-- etc.
```

Furthermore, most constructors take an extra "extension" field which can
contain different information in different stages, influenced by the parameter
`p`.  In almost all cases, after the parsing step that field is the
trivial type `data NoExt = NoExt`.  (For more details, see the [Trees that
Grow] paper. GHC versions earlier than 8.4 used a similar `PlaceHolder` type.).
This extra data makes code generation more verbose.

`ghc-source-gen` automatically sets the `NoExt` value (or equivalent) for the
terms that it generates, hiding that detail from its external API.  It also
sets and hides other fields that are irrelevant to parsing or pretty-printing,
such as simplifier ticks.

[Trees that Grow]: https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow

### Source Locations
GHC carefully tracks the source location of (nearly) every node in the AST.
That information is very useful for error reporting.  However, it would be too
verbose to set it explicitly for each individual node during code generation.
Furthermore, GHC doesn't use the source location when pretty-printing its
output, which is `ghc-source-gen`'s main use case.

Currently, `ghc-source-gen` gives to each node it generates a trivial location
without an explicit line or column.

### Parentheses
GHC represents parentheses explicitly in its syntax tree, so that it can print code
exactly as it was parsed.  Unfortunately, this means that its pretty-printing code
expects those parentheses to be present, and outputs incorrect
source code if they are missing.  `ghc-source-gen`
adds parentheses automatically in the code that it generates.

For example, consider a simplified expression syntax:

```haskell
data Expr
    = VarE String      -- ^ Variables
    | App Expr Expr    -- ^ Function application
    | Paren Expr Expr  -- ^ Parentheses
```

Then GHC would pretty-print as `"f (g x)"` the tree

```haskell
App (VarE "f") $ Paren $ App (VarE "g") (VarE "h")
```

But without the explicit parenthesis, it would pretty-print as `"f g x"`:

```haskell
App (VarE "f") $ App (VarE "g") (VarE "h")
```

which misrepresents the precedence between the two function applications.

`ghc-source-gen` resolves this issue by inserting parentheses automatically, and only when necessary.  In the expression

```haskell
var "f" @@ (var "g" @@ var "h")
```

it inserts a parenthesis automatically so that the result pretty-prints to `"f (g x)"` as expected.

GHC uses a similar approach internally itself.  For more discussion, see tickets [14289](https://gitlab.haskell.org/ghc/ghc/issues/14289) and [15738](https://gitlab.haskell.org/ghc/ghc/issues/15738).
