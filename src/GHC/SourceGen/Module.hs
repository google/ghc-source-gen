{-# LANGUAGE CPP #-}
-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell modules,
-- including import and export statements.
module GHC.SourceGen.Module
    ( -- * HsModule
      HsModule'
    , module'
      -- * Import declarations
    , ImportDecl'
    , qualified'
    , as'
    , import'
    , exposing
    , hiding
    , source
      -- * Imported/exported things
    , IE'
    , thingAll
    , thingWith
    , moduleContents
    )  where

import GHC.Hs.ImpExp (LIEWrappedName, IEWildcard(..), IEWrappedName(..), IE(..))
import GHC.Hs
    ( HsModule(..)
    , ImportDecl(..)
#if MIN_VERSION_ghc(8,10,0)
    , ImportDeclQualifiedStyle(..)
#endif
    )


import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Name
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
#if MIN_VERSION_ghc(9,0,1)
import GHC.Types.Name.Reader (RdrName)
import GHC.Plugins (IsBootInterface(NotBoot), LayoutInfo (NoLayoutInfo))
import GHC.Unit.Types (IsBootInterface(IsBoot))
#else
import RdrName (RdrName)
#endif

module'
    :: Maybe ModuleNameStr
    -> Maybe [IE'] -- ^ Exports
    -> [ImportDecl']
    -> [HsDecl']
    -> HsModule'
module' name exports imports decls = HsModule
    { hsmodName = fmap (builtLoc . unModuleNameStr) name
#if MIN_VERSION_ghc(9,0,1)
    , hsmodLayout = NoLayoutInfo 
#endif
    , hsmodExports = fmap (builtLoc . map builtLoc) exports
    , hsmodImports = map builtLoc imports
    , hsmodDecls = fmap builtLoc decls
    , hsmodDeprecMessage = Nothing
    , hsmodHaddockModHeader = Nothing
    }

qualified' :: ImportDecl' -> ImportDecl'
qualified' d = d { ideclQualified =
#if MIN_VERSION_ghc(8,10,0)
    QualifiedPre
#else
    True
#endif
}

as' :: ImportDecl' -> ModuleNameStr -> ImportDecl'
as' d m = d { ideclAs = Just (builtLoc $ unModuleNameStr m) }

import' :: ModuleNameStr -> ImportDecl'
import' m =
    noSourceText
        (noExt ImportDecl)
        (builtLoc $ unModuleNameStr m)
        Nothing
#if MIN_VERSION_ghc(9,0,1)
        NotBoot
#else
        False
#endif
        False
#if MIN_VERSION_ghc(8,10,0)
        NotQualified
#else
        False
#endif
        False
        Nothing
        Nothing

exposing :: ImportDecl' -> [IE'] -> ImportDecl'
exposing d ies = d
    { ideclHiding = Just (False, builtLoc $ map builtLoc ies) }

hiding :: ImportDecl' -> [IE'] -> ImportDecl'
hiding d ies = d
    { ideclHiding = Just (True, builtLoc $ map builtLoc ies) }

-- | Adds the @{-# SOURCE #-}@ pragma to an import.
source :: ImportDecl' -> ImportDecl'
#if MIN_VERSION_ghc(9,0,1)
source d = d { ideclSource = IsBoot }
#else
source d = d { ideclSource = True }
#endif

-- | Exports all methods and/or constructors.
--
-- > A(..)
-- > =====
-- > thingAll "A"
thingAll :: RdrNameStr -> IE'
thingAll = noExt IEThingAll . wrappedName

-- | Exports specific methods and/or constructors.
--
-- > A(b, C)
-- > =====
-- > thingWith "A" ["b", "C"]
thingWith :: RdrNameStr -> [OccNameStr] -> IE'
thingWith n cs = noExt IEThingWith (wrappedName n) NoIEWildcard
                    (map (wrappedName . unqual) cs)
                    -- The parsing step leaves the list of fields empty
                    -- and lumps them all together with the above list of
                    -- constructors.
                    []

-- TODO: support "mixed" syntax with both ".." and explicit names.

wrappedName :: RdrNameStr -> LIEWrappedName RdrName
wrappedName = builtLoc . IEName . exportRdrName

-- | Exports an entire module.
--
-- Note: this is not valid inside of an import list.
--
-- > module M
-- > =====
-- > moduleContents "M"
moduleContents :: ModuleNameStr -> IE'
moduleContents = noExt IEModuleContents . builtLoc . unModuleNameStr
