-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
-- | This module provides combinators for constructing Haskell expressions.
module GHC.SourceGen.Expr
    ( HsExpr'
    , overLabel
    , let'
    , case'
    , lambda
    , lambdaCase
    , if'
    , multiIf
    , do'
    , Stmt'
    , (@::@)
    , tyApp
    , recordConE
    , recordUpd
    ) where

import HsExpr
import HsPat (HsRecField'(..), HsRecFields(..))
import HsTypes (FieldOcc(..), AmbiguousFieldOcc(..))
import Data.String (fromString)
import SrcLoc (unLoc, GenLocated(..), Located)

import GHC.SourceGen.Binds.Internal
import GHC.SourceGen.Binds
import GHC.SourceGen.Expr.Internal
import GHC.SourceGen.Name.Internal
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
lambda ps e = noExt HsLam $ matchGroup LambdaExpr [match ps e]

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
do' = withPlaceHolder . noExt HsDo DoExpr
        . builtLoc . map (builtLoc . parenthesizeIfLet)
  where
  -- Put parentheses around a "let" in a do-binding, to avoid:
  --   do let x = ...
  --      in x
  -- which is not valid Haskell.
#if MIN_VERSION_ghc(8,6,0)
    parenthesizeIfLet (BodyStmt ext e@(L _ HsLet{}) x y)
        = BodyStmt ext (parExpr e) x y
#else
    parenthesizeIfLet (BodyStmt e@(L _ HsLet{}) x y tc)
        = BodyStmt (parExpr e) x y tc
#endif
    parenthesizeIfLet s = s

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
tyApp e t = noExt HsAppType e' t'
#elif MIN_VERSION_ghc(8,6,0)
tyApp e t = HsAppType t' e'
#else
tyApp e t = HsAppType e' t'
#endif
  where
    t' = wcType $ unLoc $ parenthesizeTypeForApp $ builtLoc t
    e' = parenthesizeExprForApp $ builtLoc e

-- | Constructs a record with explicit field names.
--
-- > A { x = y }
-- > =====
-- > recordConE "A" [("x", var "y")]
recordConE :: RdrNameStr -> [(RdrNameStr, HsExpr')] -> HsExpr'
recordConE c fs = (withPlaceHolder $ noExt RecordCon (valueRdrName c))
#if !MIN_VERSION_ghc(8,6,0)
                    noPostTcExpr
#endif
                    $ HsRecFields (map recField fs)
                        Nothing -- No ".."
  where
    recField :: (RdrNameStr, HsExpr') -> LHsRecField' (Located HsExpr')
    recField (f, e) =
        builtLoc HsRecField
            { hsRecFieldLbl =
                  builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hsRecFieldArg = builtLoc e
            , hsRecPun = False
            }

-- | Updates a record expression with explicit field names.
--
-- > r {a = b, c = d}
-- > =====
-- > recordUpd (var "x") [("a", var "b", ("c", var "d"))]
--
-- > (f x) {a = b}
-- > =====
-- > recordUpd (var "f" @@ var "x") [("a", var "b")]
--
-- > f x {a = b} -- equivalent to f (x {a = b})
-- > =====
-- > var "f" @@ recordUpd (var "x") [("a", var "b")]
recordUpd :: HsExpr' -> [(RdrNameStr, HsExpr')] -> HsExpr'
recordUpd e fs =
    withPlaceHolder4
       $ noExt RecordUpd (parenthesizeExprForApp $ builtLoc e)
       $ map mkField fs
  where
    mkField :: (RdrNameStr, HsExpr') -> LHsRecUpdField'
    mkField (f, e') =
        builtLoc HsRecField
            { hsRecFieldLbl =
                builtLoc $ withPlaceHolder $ noExt Ambiguous $ valueRdrName f
            , hsRecFieldArg = builtLoc e'
            , hsRecPun = False
            }
    withPlaceHolder4 = withPlaceHolder . withPlaceHolder . withPlaceHolder
                            . withPlaceHolder
