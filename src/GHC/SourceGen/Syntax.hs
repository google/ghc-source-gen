-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module defines type synonyms for the different parts of GHC's syntax
tree.

GHC uses the same types at different stages of the compilation, distinguishing
them using a type parameter.
The functions in @ghc-source-gen@ construct values as they would appear after the
parsing step.
-}
module GHC.SourceGen.Syntax where

import HsSyn
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
    )
import HsBinds (Sig, HsLocalBinds)
import HsPat
import RdrName (RdrName)
import SrcLoc (Located)

#if MIN_VERSION_ghc(8,4,0)
import HsExtension (GhcPs)
#endif

#if MIN_VERSION_ghc(8,4,0)
type HsExpr' = HsExpr GhcPs
type HsLit' = HsLit GhcPs
type HsType' = HsType GhcPs
type HsDecl' = HsDecl GhcPs
type HsModule' = HsModule GhcPs
type HsBind' = HsBind GhcPs
type HsLocalBinds' = HsLocalBinds GhcPs
type HsValBinds' = HsValBinds GhcPs
type Sig' = Sig GhcPs
type Pat' = Pat GhcPs
type HsMatchContext' = HsMatchContext RdrName
type Match' = Match GhcPs
type MatchGroup' = MatchGroup GhcPs
type GRHS' = GRHS GhcPs
type GRHSs' = GRHSs GhcPs
type Stmt' = Stmt GhcPs (Located HsExpr')
type HsTyVarBndr' = HsTyVarBndr GhcPs
type HsOverLit' = HsOverLit GhcPs
type LHsQTyVars' = LHsQTyVars GhcPs
type ConDecl' = ConDecl GhcPs
type HsConDeclDetails' = HsConDeclDetails GhcPs
type LHsSigType' = LHsSigType GhcPs
type IE' = IE GhcPs
type ImportDecl' = ImportDecl GhcPs
type LHsSigWcType' = LHsSigWcType GhcPs
type LHsWcType' = LHsWcType GhcPs

#else
type HsExpr' = HsExpr RdrName
type HsLit' = HsLit
type HsType' = HsType RdrName
type HsDecl' = HsDecl RdrName
type HsModule' = HsModule RdrName
type HsBind' = HsBind RdrName
type HsLocalBinds' = HsLocalBinds RdrName
type HsValBinds' = HsValBinds RdrName
type Sig' = Sig RdrName
type Pat' = Pat RdrName
type HsMatchContext' = HsMatchContext RdrName
type Match' = Match RdrName
type MatchGroup' = MatchGroup RdrName
type GRHS' = GRHS RdrName
type GRHSs' = GRHSs RdrName
type Stmt' = Stmt RdrName (Located HsExpr')
type HsTyVarBndr' = HsTyVarBndr RdrName
type HsOverLit' = HsOverLit RdrName
type LHsQTyVars' = LHsQTyVars RdrName
type ConDecl' = ConDecl RdrName
type HsConDeclDetails' = HsConDeclDetails RdrName
type LHsSigType' = LHsSigType RdrName
type IE' = IE RdrName
type ImportDecl' = ImportDecl RdrName
type LHsSigWcType' = LHsSigWcType RdrName
type LHsWcType' = LHsWcType RdrName

#endif
