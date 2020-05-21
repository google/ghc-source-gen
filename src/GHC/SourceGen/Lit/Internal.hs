-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Lit.Internal where

import BasicTypes (SourceText(NoSourceText), FractionalLit(..))
#if MIN_VERSION_ghc(8,4,0)
import BasicTypes (IntegralLit(..))
#endif
import GHC.Hs.Lit
import GHC.SourceGen.Syntax.Internal

noSourceText :: (SourceText -> a) -> a
noSourceText = ($ NoSourceText)

litNeedsParen :: HsLit' -> Bool
-- For now, ignoring cases that only arrive from internal compiler passes.
-- Furthermore, GHC parses primitive numbers like -3.0# without needing parentheses.
-- So we can uniformly ignore this step.
litNeedsParen _ = False

overLitNeedsParen :: HsOverLit' -> Bool
overLitNeedsParen = needs . ol_val
  where
#if MIN_VERSION_ghc(8,4,0)
    needs (HsIntegral x) = il_neg x
    needs (HsFractional x) = fl_neg x
#else
    needs (HsIntegral _ x) = x < 0
    needs (HsFractional x) = fl_value x < 0
#endif
    needs _ = False
