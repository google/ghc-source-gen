-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module defines custom types for defining names of various
-- syntax terms.
--
-- These types are all instances of 'Data.String.IsString'.  For ease of use,
-- we recommend enabling the @OverloadedStrings@ extension.
module GHC.SourceGen.Name
    ( RdrNameStr(..)
    , RawNameSpace(..)
    , rdrNameStrToString
    , OccNameStr(..)
    , occNameStrToString
    , ModuleNameStr(..)
    , moduleNameStrToString
    , qual
    , unqual
    ) where

import FastString (unpackFS)
import Module (moduleNameString)
import GHC.SourceGen.Name.Internal

unqual :: OccNameStr -> RdrNameStr
unqual = UnqualStr

qual :: ModuleNameStr -> OccNameStr -> RdrNameStr
qual = QualStr

moduleNameStrToString :: ModuleNameStr -> String
moduleNameStrToString = moduleNameString . unModuleNameStr

occNameStrToString :: OccNameStr -> String
occNameStrToString (OccNameStr _ s) = unpackFS s

rdrNameStrToString :: RdrNameStr -> String
rdrNameStrToString (UnqualStr o) = occNameStrToString o
rdrNameStrToString (QualStr m o) =
    moduleNameStrToString m ++ '.' : occNameStrToString o
