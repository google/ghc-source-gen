-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell modules,
-- including import and export statements.
module GHC.SourceGen.Module
    ( HsModule'
    , IE'
    , ImportDecl'
    , module'
    , qualified'
    , as'
    , import'
    , exposing
    , hiding
    )  where

import HsSyn
    ( HsModule(..)
    , ImportDecl(..)
    )

import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)

module'
    :: Maybe ModuleNameStr
    -> Maybe [IE'] -- ^ Exports
    -> [ImportDecl']
    -> [HsDecl']
    -> HsModule'
module' name exports imports decls = HsModule
    { hsmodName = fmap (builtLoc . unModuleNameStr) name
    , hsmodExports = fmap (builtLoc . map builtLoc) exports
    , hsmodImports = map builtLoc imports
    , hsmodDecls = fmap builtLoc decls
    , hsmodDeprecMessage = Nothing
    , hsmodHaddockModHeader = Nothing
    }

qualified' :: ImportDecl' -> ImportDecl'
qualified' d = d { ideclQualified = True }

as' :: ImportDecl' -> ModuleNameStr -> ImportDecl'
as' d m = d { ideclAs = Just (builtLoc $ unModuleNameStr m) }

import' :: ModuleNameStr -> ImportDecl'
import' m = noSourceText (noExt ImportDecl)
            (builtLoc $ unModuleNameStr m)
            Nothing False False False False Nothing Nothing

exposing :: ImportDecl' -> [IE'] -> ImportDecl'
exposing d ies = d
    { ideclHiding = Just (False, builtLoc $ map builtLoc ies) }

hiding :: ImportDecl' -> [IE'] -> ImportDecl'
hiding d ies = d
    { ideclHiding = Just (True, builtLoc $ map builtLoc ies) }
