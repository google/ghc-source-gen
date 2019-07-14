-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module defines custom types for defining names of various
-- syntax terms.
--
-- These types are all instances of 'Data.String.IsString'.  For easier use,
-- we recommend enabling the @OverloadedStrings@ extension.
module GHC.SourceGen.Name
    ( RdrNameStr(..)
    , OccNameStr
    , ModuleNameStr(..)
    , qual
    , unqual
    ) where

import GHC.SourceGen.Name.Internal

unqual :: OccNameStr -> RdrNameStr
unqual = RawUnqual

qual :: ModuleNameStr -> OccNameStr -> RdrNameStr
qual = RawQual
