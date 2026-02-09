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
{-# LANGUAGE CPP #-}
module GHC.SourceGen.Name
    ( -- * RdrNameStr
      RdrNameStr(..)
    , RawNameSpace(..)
    , rdrNameStrToString
    , qual
    , unqual
      -- * OccNameStr
    , OccNameStr
    , occNameStrToString
    , occNameStrNamespace
    , occNameToStr
    , nameToStr
      -- ModuleNameStr
    , ModuleNameStr(..)
    , moduleNameStrToString
    ) where

import GHC.Data.FastString (unpackFS)
import GHC.Unit.Module (moduleNameString)
import GHC.Types.Name.Occurrence (OccName, occNameFS, occNameSpace, isVarNameSpace)
import GHC.Types.Name (Name, nameOccName)
import GHC.SourceGen.Name.Internal

unqual :: OccNameStr -> RdrNameStr
unqual = UnqualStr

qual :: ModuleNameStr -> OccNameStr -> RdrNameStr
qual = QualStr

moduleNameStrToString :: ModuleNameStr -> String
moduleNameStrToString = moduleNameString . unModuleNameStr

occNameStrToString :: OccNameStr -> String
occNameStrToString (OccNameStr _ s) = unpackFS s

occNameStrNamespace :: OccNameStr -> RawNameSpace
occNameStrNamespace (OccNameStr n _) = n

rdrNameStrToString :: RdrNameStr -> String
rdrNameStrToString (UnqualStr o) = occNameStrToString o
rdrNameStrToString (QualStr m o) =
    moduleNameStrToString m ++ '.' : occNameStrToString o

-- | Converts a GHC 'OccName' to an 'OccNameStr'.  Ignores whether the input
-- came from the namespace of types or of values.
occNameToStr :: OccName -> OccNameStr
occNameToStr o = OccNameStr n (occNameFS o)
  where
    n = if isVarNameSpace $ occNameSpace o
            then Value
            else Constructor

-- | Converts from a GHC 'Name' to an 'OccNameStr'.  Ignores whether
-- the input came from the namespace of types or of values, as well
-- as any other information about where the name came from.
nameToStr :: Name -> OccNameStr
nameToStr = occNameToStr  . nameOccName
