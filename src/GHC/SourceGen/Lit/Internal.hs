-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Lit.Internal where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceText (SourceText(NoSourceText), FractionalLit(..), IntegralLit(..))
#else
import GHC.Types.Basic (SourceText(NoSourceText), FractionalLit(..), IntegralLit(..))
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
    needs (HsIntegral x) = il_neg x
    needs (HsFractional x) = fl_neg x
    needs _ = False
