-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell literals,
-- which may be used in either patterns or expressions.
{-# LANGUAGE CPP #-}
module GHC.SourceGen.Lit
    ( HsLit'
    , HsOverLit'
    , HasLit(..)
    , char
    , string
    , int
    , frac
    ) where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceText (mkTHFractionalLit, mkIntegralLit)
import GHC.Data.FastString (fsLit)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (mkFractionalLit, mkIntegralLit)
import GHC.Data.FastString (fsLit)
#else
import BasicTypes (mkFractionalLit, mkIntegralLit)
import FastString (fsLit)
#endif
import GHC.Hs.Lit
import GHC.Hs.Expr (noExpr, noSyntaxExpr, HsExpr(..))
import GHC.Hs.Pat (Pat(..))

import GHC.SourceGen.Lit.Internal
import GHC.SourceGen.Syntax.Internal

class HasLit e where
    lit :: HsLit' -> e
    overLit :: HsOverLit' -> e

instance HasLit HsExpr' where
    lit = withEpAnnNotUsed HsLit
    overLit = withEpAnnNotUsed HsOverLit

instance HasLit Pat' where
    lit = noExt LitPat
    overLit l = withPlaceHolder
                    $ withEpAnnNotUsed NPat (builtLoc l) Nothing noSyntaxExpr

char :: HasLit e => Char -> e
char = lit . noSourceText HsChar

string :: HasLit e => String -> e
string = lit . noSourceText HsString . fsLit

-- | Note: this is an *overloaded* integer.
int :: HasLit e => Integer -> e
int n = overLit $ withPlaceHolder $ withPlaceHolder (noExt OverLit n') noExpr
  where
    n' = HsIntegral $ mkIntegralLit n

-- | Note: this is an *overloaded* rational, e.g., a decimal number.
frac :: HasLit e => Rational -> e
frac x = overLit $ withPlaceHolder $ withPlaceHolder (noExt OverLit $ HsFractional x') noExpr
  where
#if MIN_VERSION_ghc(9,2,0)
    x' = mkTHFractionalLit x
#else
    x' = mkFractionalLit x
#endif
