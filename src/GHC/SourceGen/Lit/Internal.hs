-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

module GHC.SourceGen.Lit.Internal where

import BasicTypes (SourceText(NoSourceText))

noSourceText :: (SourceText -> a) -> a
noSourceText = ($ NoSourceText)
