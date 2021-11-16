-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Type.Internal where

import GHC.Hs (GhcPs)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs.Type as Types
import GHC.Types.SrcLoc (unLoc)
#else
import GHC.Hs.Type as Types
import SrcLoc (unLoc)
#endif

import GHC.SourceGen.Syntax.Internal

mkQTyVars :: [HsTyVarBndr'] -> LHsQTyVars'
mkQTyVars vars =  withPlaceHolder
                $ noExt (withPlaceHolder HsQTvs)
                $ map mkLocated vars

sigType :: HsType' -> LHsSigType'
#if MIN_VERSION_ghc(9,2,0)
sigType = mkLocated . noExt HsSig (noExt HsOuterImplicit) . mkLocated
#else
sigType = withPlaceHolder . noExt (withPlaceHolder Types.HsIB) . builtLoc
#endif


-- TODO: GHC >= 8.6 provides parenthesizeHsType.  For consistency with
-- older versions, we're implementing our own parenthesis-wrapping.
-- Once we stop supporting GHC-8.4, we can switch to that implementation.

parenthesizeTypeForApp, parenthesizeTypeForOp, parenthesizeTypeForFun
    :: LHsType GhcPs -> LHsType GhcPs
parenthesizeTypeForApp t
    | needsParenForApp (unLoc t) = parTy t
    | otherwise = t
parenthesizeTypeForOp t
    | needsParenForOp (unLoc t) = parTy t
    | otherwise = t
parenthesizeTypeForFun t
    | needsParenForFun (unLoc t) = parTy t
    | otherwise = t

needsParenForFun, needsParenForOp, needsParenForApp
    :: HsType' -> Bool
needsParenForFun t = case t of
    HsForAllTy{} -> True
    HsQualTy{} -> True
    HsFunTy{} -> True
    _ -> False
needsParenForOp t = case t of
    HsOpTy{} -> True
    _ -> needsParenForFun t
needsParenForApp t = case t of
    HsAppTy {} -> True
    _ -> needsParenForOp t

parTy :: LHsType GhcPs -> LHsType GhcPs
parTy = mkLocated . withEpAnnNotUsed HsParTy

sigWcType :: HsType' -> LHsSigWcType'
sigWcType = noExt (withPlaceHolder Types.HsWC) . sigType

wcType :: HsType' -> LHsWcType'
wcType = noExt (withPlaceHolder Types.HsWC) . mkLocated

patSigType :: HsType' -> HsPatSigType'
#if MIN_VERSION_ghc(9,2,0)
patSigType = withEpAnnNotUsed mkHsPatSigType . mkLocated
#elif MIN_VERSION_ghc(9,0,0)
patSigType = mkHsPatSigType . builtLoc
#else
patSigType = sigWcType
#endif
