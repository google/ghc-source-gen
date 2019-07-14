-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
-- | This module provides combinators for constructing Haskell expressions.
module GHC.SourceGen.Expr where

import HsExpr
import Data.String (fromString)
import SrcLoc (unLoc)

import GHC.SourceGen.Binds.Internal
import GHC.SourceGen.Binds
import GHC.SourceGen.Syntax
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal
    ( parenthesizeTypeForApp
    , sigWcType
    , wcType
    )

-- | An overloaded label, as used with the @OverloadedLabels@ extension.
--
-- > #foo
-- > =====
-- > overLabel "foo"
overLabel :: String -> HsExpr'
overLabel = noExt HsOverLabel Nothing . fromString

let' :: [RawValBind] -> HsExpr' -> HsExpr'
let' binds e = noExt HsLet (builtLoc $ valBinds binds) $ builtLoc e

case' :: HsExpr' -> [RawMatch] -> HsExpr'
case' e matches = noExt HsCase (builtLoc e)
                    $ matchGroup CaseAlt matches

lambda :: [Pat'] -> HsExpr' -> HsExpr'
lambda ps e = noExt HsLam $ matchGroup LambdaExpr [matchRhs ps e]

lambdaCase :: [RawMatch] -> HsExpr'
lambdaCase = noExt HsLamCase . matchGroup CaseAlt

if' :: HsExpr' -> HsExpr' -> HsExpr' -> HsExpr'
if' x y z = noExt HsIf Nothing (builtLoc x) (builtLoc y) (builtLoc z)

-- | A MultiWayIf expression.
--
-- > if | f x = "f"
-- >    | g x = "g"
-- >    | otherwise = "h"
-- > =====
-- > multiIf
-- >     [ guardedStmt (var "f" @@ var "x") $ rhs (string "f")
-- >     , guardedStmt (var "g" @@ var "x") $ rhs (string "g")
-- >     , guardedStmt (var "otherwise") $ rhs (string "h")
-- >     ]
multiIf :: [GuardedExpr] -> HsExpr'
multiIf = noExtOrPlaceHolder HsMultiIf . map builtLoc

-- | A do-expression.
--
-- Individual statements may be constructed with '<--' and/or 'stmt'.
--
-- > do
-- >   x <- act
-- >   return x
-- > =====
-- > do' [var "x" <-- var "act", stmt $ var "return" @@ var "x"]
do' :: [Stmt'] -> HsExpr'
do' = withPlaceHolder . noExt HsDo DoExpr . builtLoc . map builtLoc

-- | A type constraint on an expression.
--
-- > e :: t
-- > =====
-- > var "e" @::@ var "t"
(@::@) :: HsExpr' -> HsType' -> HsExpr'
#if MIN_VERSION_ghc(8,8,0)
e @::@ t = noExt ExprWithTySig (builtLoc e) (sigWcType t)
#elif MIN_VERSION_ghc(8,6,0)
e @::@ t = ExprWithTySig (sigWcType t) (builtLoc e)
#else
e @::@ t = ExprWithTySig (builtLoc e) (sigWcType t)
#endif
-- TODO: The Outputable instance prepends extra spaces; I'm not sure why.

-- | Explicit type application.
--
-- > f @ Int
-- > =====
-- > var "f" @@ var "Int"
tyApp :: HsExpr' -> HsType' -> HsExpr'
#if MIN_VERSION_ghc(8,8,0)
tyApp e t = noExt HsAppType (builtLoc e) t'
#elif MIN_VERSION_ghc(8,6,0)
tyApp e t = HsAppType t' (builtLoc e)
#else
tyApp e t = HsAppType (builtLoc e) t'
#endif
  where
    t' = wcType $ unLoc $ parenthesizeTypeForApp $ builtLoc t
