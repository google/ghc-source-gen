-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Expr.Internal where

import HsExpr
import SrcLoc (Located, unLoc)

import GHC.SourceGen.Lit.Internal
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
    HsAppType{} -> True
    HsStatic{} -> True
    _ -> needsExprForOp e

