-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Expr.Internal where

import GHC.Hs.Expr
import GHC.Types.SrcLoc (unLoc)

import GHC.SourceGen.Lit.Internal
import GHC.SourceGen.Syntax.Internal

#if MIN_VERSION_ghc(9,10,0)
import GHC.Parser.Annotation (EpToken (..), noSpanAnchor)
#endif

#if MIN_VERSION_ghc(9,4,0)
import Language.Haskell.Syntax.Extension
#endif

parenthesizeExprForApp, parenthesizeExprForOp
    :: LHsExpr' -> LHsExpr'
parenthesizeExprForApp e 
    | needsExprForApp (unLoc e) = parExpr e
    | otherwise = e
parenthesizeExprForOp e
    | needsExprForOp (unLoc e) = parExpr e
    | otherwise = e

parExpr :: LHsExpr' -> LHsExpr'
#if MIN_VERSION_ghc(9,10,0)
parExpr e = mkLocated $ HsPar (EpTok noSpanAnchor, EpTok noSpanAnchor) e
#elif MIN_VERSION_ghc(9,4,0)
parExpr e = mkLocated $ withEpAnnNotUsed HsPar mkToken e mkToken
#else
parExpr = mkLocated . withEpAnnNotUsed HsPar
#endif

needsExprForApp, needsExprForOp :: HsExpr' -> Bool
needsExprForOp e = case e of
    -- TODO: more care for literals; only needed for negative numbers?
    HsLit _ l -> litNeedsParen l
    HsOverLit _ l -> overLitNeedsParen l
    HsLam{} -> True
#if !MIN_VERSION_ghc(9,10,0)
    HsLamCase{} -> True
#endif
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

