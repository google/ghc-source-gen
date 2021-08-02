-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Type.Internal where

#if MIN_VERSION_ghc(9,0,1)
import GHC.Hs.Type as Types
import GHC.Types.SrcLoc (Located, unLoc)
#else
import GHC.Hs.Types as Types
import SrcLoc (Located, unLoc)
#endif

import GHC.SourceGen.Syntax.Internal

mkQTyVars :: [HsTyVarBndrUnit'] -> LHsQTyVars'
mkQTyVars vars =  withPlaceHolder
                $ noExt (withPlaceHolder HsQTvs)
                $ map builtLoc vars

sigType :: HsType' -> LHsSigType'
sigType = implicitBndrs . builtLoc

implicitBndrs :: t -> HsImplicitBndrs' t
implicitBndrs = withPlaceHolder . noExt (withPlaceHolder Types.HsIB)


-- TODO: GHC >= 8.6 provides parenthesizeHsType.  For consistency with
-- older versions, we're implementing our own parenthesis-wrapping.
-- Once we stop supporting GHC-8.4, we can switch to that implementation.

parenthesizeTypeForApp, parenthesizeTypeForOp, parenthesizeTypeForFun
    :: Located HsType' -> Located HsType'
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

parTy :: Located HsType' -> Located HsType'
parTy = builtLoc . noExt HsParTy

sigWcType :: HsType' -> LHsSigWcType'
sigWcType = noExt (withPlaceHolder Types.HsWC) . sigType

wcType :: HsType' -> LHsWcType'
wcType = noExt (withPlaceHolder Types.HsWC) . builtLoc

#if MIN_VERSION_ghc(9,0,1)
patSigType :: HsType' -> HsPatSigType'
patSigType = noExt HsPS . builtLoc
#endif
