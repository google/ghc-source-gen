-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.SourceGen.Syntax.Internal where


import GHC.Hs
    ( HsDecl
    , HsExpr(..)
    , HsLit
    , HsModule
    , HsType(..)
    , HsBind
    , HsTyVarBndr
    , HsOverLit
    , HsValBinds
    , HsMatchContext
    , IE
    , LHsQTyVars
    , Match
    , MatchGroup
    , GRHS
    , GRHSs
    , Stmt
    , ConDecl
    , HsConDeclDetails
    , LHsSigType
    , ImportDecl
    , LHsSigWcType
    , LHsWcType
    , HsImplicitBndrs
    , TyFamInstDecl
#if !MIN_VERSION_ghc(8,8,0)
    , LHsRecField
    , LHsRecUpdField
#endif
    )
import GHC.Hs.Binds (Sig, HsLocalBinds)
#if MIN_VERSION_ghc(8,6,0)
import GHC.Hs.Decls (DerivStrategy)
#else
import BasicTypes (DerivStrategy)
#endif
import GHC.Hs.Decls (HsDerivingClause)
import GHC.Hs.Pat
import RdrName (RdrName)
import SrcLoc (SrcSpan, Located, GenLocated(..), mkGeneralSrcSpan)

#if MIN_VERSION_ghc(8,8,0)
import BasicTypes (PromotionFlag(..))
#else
import GHC.Hs.Types (Promoted(..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Extension (NoExtField(NoExtField))
#elif MIN_VERSION_ghc(8,6,0)
import GHC.Hs.Extension (NoExt(NoExt))
#else
import PlaceHolder(PlaceHolder(..))
#endif

import GHC.Hs.Extension (GhcPs)

#if MIN_VERSION_ghc(8,6,0)
#if MIN_VERSION_ghc(8,10,0)
noExt :: (NoExtField -> a) -> a
noExt = ($ NoExtField)

noExtOrPlaceHolder :: (NoExtField -> a) -> a
noExtOrPlaceHolder = noExt

#else
noExt :: (NoExt -> a) -> a
noExt = ($ NoExt)

noExtOrPlaceHolder :: (NoExt -> a) -> a
noExtOrPlaceHolder = noExt
#endif

withPlaceHolder :: a -> a
withPlaceHolder = id

withPlaceHolders :: a -> a
withPlaceHolders = id

#else

noExt :: a -> a
noExt = id

noExtOrPlaceHolder :: (PlaceHolder -> a) -> a
noExtOrPlaceHolder = withPlaceHolder

withPlaceHolder :: (PlaceHolder -> a) -> a
withPlaceHolder = ($ PlaceHolder)

withPlaceHolders :: ([PlaceHolder] -> a) -> a
withPlaceHolders = ($ [])

#endif

builtSpan :: SrcSpan
builtSpan = mkGeneralSrcSpan "<ghc-source-gen>"

builtLoc :: e -> Located e
builtLoc = L builtSpan

-- In GHC-8.8.* (but not >=8.10 or <=8.6), source locations for Pat aren't
-- stored in each node, and LPat is a synonym for Pat.
builtPat :: Pat' -> LPat'
#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,10,0)
builtPat = id
#else
builtPat = builtLoc
#endif

#if MIN_VERSION_ghc(8,8,0)
promoted, notPromoted :: PromotionFlag
promoted = IsPromoted
notPromoted = NotPromoted
#else
promoted, notPromoted :: Promoted
promoted = Promoted
notPromoted = NotPromoted
#endif

-- TODO: these Haddock cross-references don't link to the actual
-- definition, only to the module they come from.  I think it's
-- because the classes aren't in scope here.

-- | A Haskell type, as it is represented after the parsing step.
--
-- Instances:
--
-- * 'GHC.SourceGen.Overloaded.BVar'
-- * 'GHC.SourceGen.Overloaded.Var'
-- * 'GHC.SourceGen.Overloaded.Par'
-- * 'GHC.SourceGen.Overloaded.App'
-- * 'GHC.SourceGen.Overloaded.HasTuple'
type HsType' = HsType GhcPs

-- | A Haskell pattern, as it is represented after the parsing step.
--
-- Instances:
--
-- * 'GHC.SourceGen.Overloaded.BVar'
-- * 'GHC.SourceGen.Overloaded.Par'
-- * 'GHC.SourceGen.Overloaded.HasTuple'
-- * 'GHC.SourceGen.Overloaded.HasList'
-- * 'GHC.SourceGen.Lit.HasLit'
type Pat' = Pat GhcPs

-- | A Haskell expression, as it is represented after the parsing step.
--
-- Instances:
--
-- * 'GHC.SourceGen.Overloaded.BVar'
-- * 'GHC.SourceGen.Overloaded.Var'
-- * 'GHC.SourceGen.Overloaded.Par'
-- * 'GHC.SourceGen.Overloaded.App'
-- * 'GHC.SourceGen.Overloaded.HasTuple'
-- * 'GHC.SourceGen.Overloaded.HasList'
-- * 'GHC.SourceGen.Lit.HasLit'
type HsExpr' = HsExpr GhcPs

-- | A Haskell declaration, as it is represented after the parsing step.
--
-- Instances:
--
-- * 'GHC.SourceGen.Binds.HasValBind'
-- * 'GHC.SourceGen.Binds.HasPatBind'
type HsDecl' = HsDecl GhcPs

-- | An imported or exported entity, as it is represented after the parsing step.
--
-- Instances:
--
-- * 'GHC.SourceGen.Overloaded.BVar'
-- * 'GHC.SourceGen.Overloaded.Var'
type IE' = IE GhcPs


-- | A type variable binding, as it is represented after the parsing step.
--
-- Construct with either 'GHC.SourceGen.Overloaded.bVar' (for regular type
-- variables) or `GHC.SourceGen.Type.kindedVar` (for kind signatures).
--
-- Instances:
--
-- * 'GHC.SourceGen.Overloaded.BVar'
type HsTyVarBndr' = HsTyVarBndr GhcPs

type HsLit' = HsLit GhcPs
type HsModule' = HsModule GhcPs
type HsBind' = HsBind GhcPs
type HsLocalBinds' = HsLocalBinds GhcPs
type HsValBinds' = HsValBinds GhcPs
type Sig' = Sig GhcPs
type HsMatchContext' = HsMatchContext RdrName
type Match' = Match GhcPs
type MatchGroup' = MatchGroup GhcPs
type GRHS' = GRHS GhcPs
type GRHSs' = GRHSs GhcPs
type Stmt' = Stmt GhcPs (Located HsExpr')
type HsOverLit' = HsOverLit GhcPs
type LHsQTyVars' = LHsQTyVars GhcPs
type ConDecl' = ConDecl GhcPs
type HsConDeclDetails' = HsConDeclDetails GhcPs
type LHsSigType' = LHsSigType GhcPs
type ImportDecl' = ImportDecl GhcPs
type LHsSigWcType' = LHsSigWcType GhcPs
type LHsWcType' = LHsWcType GhcPs
type HsDerivingClause' = HsDerivingClause GhcPs
type LHsRecField' arg = LHsRecField GhcPs arg
type LHsRecUpdField' = LHsRecUpdField GhcPs
type LPat' = LPat GhcPs
type HsImplicitBndrs' = HsImplicitBndrs GhcPs
type TyFamInstDecl' = TyFamInstDecl GhcPs

#if MIN_VERSION_ghc(8,6,0)
type DerivStrategy' = DerivStrategy GhcPs
#else
type DerivStrategy' = DerivStrategy
#endif
