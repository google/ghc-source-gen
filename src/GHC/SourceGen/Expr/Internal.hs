-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Expr.Internal where

#if MIN_VERSION_ghc(8,4,0)
import BasicTypes (IntegralLit(..))
#endif
import HsExpr
import HsLit
import SrcLoc (Located, unLoc)

import GHC.SourceGen.Syntax
import GHC.SourceGen.Syntax.Internal

parenthesizeExprForApp, parenthesizeExprForOp
    :: Located HsExpr' -> Located HsExpr'
parenthesizeExprForApp e 
    | needsExprForApp (unLoc e) = parExpr e
    | otherwise = e
parenthesizeExprForOp e
    | needsExprForOp (unLoc e) = parExpr e
    | otherwise = e

parExpr :: Located HsExpr' -> Located HsExpr'
parExpr = builtLoc . noExt HsPar

#if MIN_VERSION_ghc(8,6,0)
#define WILD_EXT _
#else
#define WILD_EXT
#endif

needsExprForApp, needsExprForOp :: HsExpr' -> Bool
needsExprForOp e = case e of
    -- TODO: more care for literals; only needed for negative numbers?
    HsLit WILD_EXT l -> litNeedsParen l
    HsOverLit WILD_EXT l -> overLitNeedsParen l
    HsLam{} -> True
    HsLamCase{} -> True
    OpApp{} -> True
    NegApp{} -> True
    HsCase{} -> True
    HsIf{} -> True
    HsMultiIf{} -> True
    HsLet{} -> True
    HsDo{} -> True
    ExprWithTySig{} -> True
    _ -> False
needsExprForApp e = case e of
    HsApp{} -> True
    HsStatic{} -> True
    _ -> needsExprForOp e

litNeedsParen :: HsLit' -> Bool
-- For now, ignoring cases that only arrive from internal compiler passes.
-- Furthermore, GHC parses primitive numbers like -3.0# without needing parentheses.
-- So we can uniformly ignore this step.
litNeedsParen _ = False

overLitNeedsParen :: HsOverLit' -> Bool
overLitNeedsParen = needs . ol_val
  where
#if MIN_VERSION_ghc(8,4,0)
    needs (HsIntegral x) = il_neg x
#else
    needs (HsIntegral _ x) = x < 0
#endif
    -- GHC shows fractional values with "%", so wrap them unconditionally.
    needs HsFractional{} = True
    needs _ = False
