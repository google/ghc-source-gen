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

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (FractionalLit(..), IntegralLit(..), SourceText(..))
import GHC.Data.FastString (fsLit)
#else
import BasicTypes (FractionalLit(..), IntegralLit(..), SourceText(..))
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
    lit = noExt HsLit
    overLit = noExt HsOverLit

instance HasLit Pat' where
    lit = noExt LitPat
    overLit l = withPlaceHolder
                    $ noExt NPat (builtLoc l) Nothing noSyntaxExpr

char :: HasLit e => Char -> e
char = lit . noSourceText HsChar

string :: HasLit e => String -> e
string = lit . noSourceText HsString . fsLit

-- | Note: this is an *overloaded* integer.
int :: HasLit e => Integer -> e
int n = overLit $ withPlaceHolder $ withPlaceHolder (noExt OverLit il) noExpr
  where
    il = HsIntegral $ noSourceText IL (n < 0) n

-- | Note: this is an *overloaded* rational, e.g., a decimal number.
frac :: HasLit e => Rational -> e
frac x = overLit $ withPlaceHolder $ withPlaceHolder (noExt OverLit $ HsFractional il) noExpr
  where
    il = FL (SourceText s) (x < 0) x
    s = show (fromRational x :: Double)
