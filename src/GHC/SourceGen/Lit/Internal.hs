-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

module GHC.SourceGen.Lit.Internal where

import BasicTypes (SourceText(NoSourceText), FractionalLit(..))
import BasicTypes (IntegralLit(..))
import HsLit
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
