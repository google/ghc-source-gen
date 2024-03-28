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
    , LHsExpr
    , LHsQTyVars
    , Match
    , MatchGroup
    , GRHS
    , GRHSs
    , Stmt
    , ConDecl
    , LHsSigType
    , ImportDecl
    , LHsSigWcType
    , LHsWcType
    , TyFamInstDecl
#if !MIN_VERSION_ghc(8,8,0)
    , LHsRecField
    , LHsRecUpdField
#endif
#if MIN_VERSION_ghc(9,0,0)
    , HsPatSigType
#endif
#if MIN_VERSION_ghc(9,2,0)
    , HsConDeclH98Details
#else
    , HsConDeclDetails
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
#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc (SrcSpan, Located, GenLocated(..), mkGeneralSrcSpan)
#else
import RdrName (RdrName)
import SrcLoc (SrcSpan, Located, GenLocated(..), mkGeneralSrcSpan)
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,6,0)
import GHC.Parser.Annotation
    ( SrcSpanAnn'(..)
    , AnnSortKey(..)
    , EpAnn(..)
    , EpAnnComments
    , emptyComments
    )
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (PromotionFlag(..))
#elif MIN_VERSION_ghc(8,8,0)
import BasicTypes (PromotionFlag(..))
#else
import GHC.Hs.Type (Promoted(..))
#endif

#if MIN_VERSION_ghc(9,8,0)
import GHC.Hs.Type (HsBndrVis)
#endif

#if MIN_VERSION_ghc(8,10,0)
import qualified GHC.Hs as GHC
#elif MIN_VERSION_ghc(8,6,0)
import qualified GHC.Hs.Extension as GHC
#else
import qualified HsExtension as GHC
import qualified PlaceHolder as GHC
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Var (Specificity)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Parser.Annotation
#endif

import GHC.Hs.Extension (GhcPs)

#if MIN_VERSION_ghc(8,10,0)
type NoExtField = GHC.NoExtField
#elif MIN_VERSION_ghc(8,6,0)
type NoExtField = GHC.NoExt
#endif

#if MIN_VERSION_ghc(8,10,0)
noExt :: (NoExtField -> a) -> a
noExt = ($ GHC.NoExtField)
#elif MIN_VERSION_ghc(8,6,0)
noExt :: (NoExtField -> a) -> a
noExt = ($ GHC.NoExt)
#else
noExt :: a -> a
noExt = id
#endif

#if MIN_VERSION_ghc(8,6,0)
noExtOrPlaceHolder :: (NoExtField -> a) -> a
noExtOrPlaceHolder = noExt
#else
noExtOrPlaceHolder :: (GHC.PlaceHolder -> a) -> a
noExtOrPlaceHolder = withPlaceHolder
#endif

#if MIN_VERSION_ghc(9,2,0)
withEpAnnNotUsed :: (EpAnn ann -> a) -> a
withEpAnnNotUsed = ($ EpAnnNotUsed)
#elif MIN_VERSION_ghc(8,6,0)
withEpAnnNotUsed :: (NoExtField -> a) -> a
withEpAnnNotUsed = noExt
#else
withEpAnnNotUsed :: a -> a
withEpAnnNotUsed = id
#endif

#if MIN_VERSION_ghc(9,2,0)
withNoAnnSortKey :: (AnnSortKey -> a) -> a
withNoAnnSortKey = ($ NoAnnSortKey)
#elif MIN_VERSION_ghc(8,6,0)
withNoAnnSortKey :: (NoExtField -> a) -> a
withNoAnnSortKey = noExt
#else
withNoAnnSortKey :: a -> a
withNoAnnSortKey = id
#endif

#if MIN_VERSION_ghc(9,2,0)
withEmptyEpAnnComments :: (EpAnnComments -> a) -> a
withEmptyEpAnnComments = ($ emptyComments)
#elif MIN_VERSION_ghc(8,6,0)
withEmptyEpAnnComments :: (NoExtField -> a) -> a
withEmptyEpAnnComments = noExt
#else
withEmptyEpAnnComments :: a -> a
withEmptyEpAnnComments = id
#endif

#if MIN_VERSION_ghc(8,6,0)
withPlaceHolder :: a -> a
withPlaceHolder = id
#else
withPlaceHolder :: (GHC.PlaceHolder -> a) -> a
withPlaceHolder = ($ GHC.PlaceHolder)
#endif

#if MIN_VERSION_ghc(8,6,0)
withPlaceHolders :: a -> a
withPlaceHolders = id
#else
withPlaceHolders :: ([GHC.PlaceHolder] -> a) -> a
withPlaceHolders = ($ [])
#endif

builtSpan :: SrcSpan
builtSpan = mkGeneralSrcSpan "<ghc-source-gen>"

builtLoc :: e -> Located e
builtLoc = L builtSpan

#if MIN_VERSION_ghc(9,2,0)
type SrcSpanAnn ann = GHC.SrcSpanAnn' (EpAnn ann)
#else
type SrcSpanAnn ann = SrcSpan
#endif

mkLocated :: a -> GenLocated (SrcSpanAnn ann) a
mkLocated = L (toAnn builtSpan)
  where
#if MIN_VERSION_ghc(9,2,0)
    toAnn = SrcSpanAnn EpAnnNotUsed
#else
    toAnn = id
#endif

-- In GHC-8.8.* (but not >=8.10 or <=8.6), source locations for Pat aren't
-- stored in each node, and LPat is a synonym for Pat.
builtPat :: Pat' -> LPat'
#if MIN_VERSION_ghc(9,2,0)
builtPat = mkLocated
#elif MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,10,0)
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

type LHsExpr' = LHsExpr GhcPs

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
#if MIN_VERSION_ghc(9,8,0)
type HsTyVarBndr' = HsTyVarBndr (HsBndrVis GhcPs) GhcPs
type HsTyVarBndrS' = HsTyVarBndr Specificity GhcPs
#elif MIN_VERSION_ghc(9,0,0)
type HsTyVarBndr' = HsTyVarBndr () GhcPs
type HsTyVarBndrS' = HsTyVarBndr Specificity GhcPs
#else
type HsTyVarBndr' = HsTyVarBndr GhcPs
type HsTyVarBndrS' = HsTyVarBndr GhcPs
#endif

type HsLit' = HsLit GhcPs
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,6,0)
type HsModule' = HsModule
#else
type HsModule' = HsModule GhcPs
#endif
type HsBind' = HsBind GhcPs
type HsLocalBinds' = HsLocalBinds GhcPs
type HsValBinds' = HsValBinds GhcPs
type Sig' = Sig GhcPs
#if MIN_VERSION_ghc(9,0,0)
type HsMatchContext' = HsMatchContext GhcPs
#else
type HsMatchContext' = HsMatchContext RdrName
#endif
type Match' = Match GhcPs
type MatchGroup' = MatchGroup GhcPs
type GRHS' = GRHS GhcPs
type GRHSs' = GRHSs GhcPs
type Stmt' = Stmt GhcPs LHsExpr'
type HsOverLit' = HsOverLit GhcPs
type LHsQTyVars' = LHsQTyVars GhcPs
type ConDecl' = ConDecl GhcPs
#if MIN_VERSION_ghc(9,2,0)
type HsConDeclDetails' = HsConDeclH98Details GhcPs
#else
type HsConDeclDetails' = HsConDeclDetails GhcPs
#endif
type LHsSigType' = LHsSigType GhcPs
type ImportDecl' = ImportDecl GhcPs
type LHsSigWcType' = LHsSigWcType GhcPs
type LHsWcType' = LHsWcType GhcPs
type HsDerivingClause' = HsDerivingClause GhcPs
type LHsRecField' arg = LHsRecField GhcPs arg
#if MIN_VERSION_ghc(9,8,0)
type LHsRecUpdField' = LHsRecUpdField GhcPs GhcPs
#else
type LHsRecUpdField' = LHsRecUpdField GhcPs
#endif
type LPat' = LPat GhcPs
type TyFamInstDecl' = TyFamInstDecl GhcPs

#if MIN_VERSION_ghc(8,6,0)
type DerivStrategy' = DerivStrategy GhcPs
#else
type DerivStrategy' = DerivStrategy
#endif

#if MIN_VERSION_ghc(9,0,0)
type HsPatSigType' = HsPatSigType GhcPs
#else
type HsPatSigType' = LHsSigWcType'
#endif

#if MIN_VERSION_ghc(9,2,0)
type LIdP = GHC.LIdP GHC.GhcPs
#else
type LIdP = Located (GHC.IdP GHC.GhcPs)
#endif

#if MIN_VERSION_ghc(9,4,0)
mkUniToken :: GenLocated TokenLocation (GHC.HsUniToken t u)
mkUniToken = L NoTokenLoc GHC.HsNormalTok
#endif

#if MIN_VERSION_ghc(9,4,0)
mkToken :: GenLocated TokenLocation (GHC.HsToken t)
mkToken = L NoTokenLoc GHC.HsTok
#endif
