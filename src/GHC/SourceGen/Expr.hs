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
    , listComp
    , Stmt'
    , (@::@)
    , tyApp
    , recordConE
    , recordUpd
    , from
    , fromThen
    , fromTo
    , fromThenTo
    ) where

import GHC.Hs.Expr
import GHC.Hs.Extension (GhcPs)
#if MIN_VERSION_ghc(9,6,0)
import GHC.Hs.Extension (noHsTok)
import GHC.Types.SourceText (SourceText(NoSourceText))
#endif
#if MIN_VERSION_ghc(9,4,0)
import GHC.Hs.Pat (HsFieldBind(..), HsRecFields(..))
#else
import GHC.Hs.Pat (HsRecField'(..), HsRecFields(..))
#endif
import GHC.Hs.Type (FieldOcc(..), AmbiguousFieldOcc(..))
import GHC.Hs.Utils (mkHsIf)
import Data.String (fromString)
#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc (unLoc, GenLocated(..))
#else
import SrcLoc (unLoc, GenLocated(..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation (EpAnn(..))
#endif

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
overLabel = hsOverLabel . fromString
  where
#if MIN_VERSION_ghc(9,6,0)
    hsOverLabel = withEpAnnNotUsed HsOverLabel NoSourceText
#elif MIN_VERSION_ghc(9,2,0)
    hsOverLabel = withEpAnnNotUsed HsOverLabel
#else
    hsOverLabel = noExt HsOverLabel Nothing
#endif

let' :: [RawValBind] -> HsExpr' -> HsExpr'
#if MIN_VERSION_ghc(9,4,0)
let' binds e = withEpAnnNotUsed HsLet mkToken (toHsLocalBinds $ valBinds binds) mkToken $ mkLocated e
#else
let' binds e = withEpAnnNotUsed HsLet (toHsLocalBinds $ valBinds binds) $ mkLocated e
#endif
  where
#if MIN_VERSION_ghc(9,2,0)
    toHsLocalBinds = id
#else
    toHsLocalBinds = builtLoc
#endif

case' :: HsExpr' -> [RawMatch] -> HsExpr'
case' e matches = withEpAnnNotUsed HsCase (mkLocated e)
                    $ matchGroup CaseAlt matches

lambda :: [Pat'] -> HsExpr' -> HsExpr'
lambda ps e = noExt HsLam $ matchGroup LambdaExpr [match ps e]

lambdaCase :: [RawMatch] -> HsExpr'
#if MIN_VERSION_ghc(9,4,0)
lambdaCase = withEpAnnNotUsed HsLamCase LamCase . matchGroup CaseAlt
#else
lambdaCase = withEpAnnNotUsed HsLamCase . matchGroup CaseAlt
#endif

if' :: HsExpr' -> HsExpr' -> HsExpr' -> HsExpr'
if' x y z = mkHsIf
                (mkLocated x)
                (mkLocated y)
                (mkLocated z)
#if MIN_VERSION_ghc(9,2,0)
                EpAnnNotUsed
#endif

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
#if MIN_VERSION_ghc(9,4,0)
multiIf = withPlaceHolder (withEpAnnNotUsed HsMultiIf) . map mkLocated
#else
multiIf = withPlaceHolder (withEpAnnNotUsed HsMultiIf) . map builtLoc
#endif

-- | A do-expression.
--
-- Individual statements may be constructed with '<--' and/or 'stmt'.
--
-- > do
-- >   x <- act
-- >   return x
-- > =====
-- > do' [bvar "x" <-- var "act", stmt $ var "return" @@ var "x"]
do' :: [Stmt'] -> HsExpr'
do' = withPlaceHolder
#if MIN_VERSION_ghc(9,0,0)
        . withEpAnnNotUsed HsDo (DoExpr Nothing)
#else
        . noExt HsDo DoExpr
#endif
        . mkLocated . map (mkLocated . parenthesizeIfLet)
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

-- | A list comprehension expression.
--
-- > [x * 2 | x <- [1 .. 10], even x]
-- > =====
-- > listComp (op (bvar "x") "*" (int 2))
-- >          [ bvar "x" <-- fromTo (int 1) (int 10)
-- >          , stmt $ var "even" @@ bvar "x"
-- >          ]
listComp :: HsExpr' -> [Stmt'] -> HsExpr'
listComp lastExpr stmts =
    let lastStmt = noExt LastStmt (mkLocated lastExpr) ret noSyntaxExpr
#if MIN_VERSION_ghc(9,0,0)
        ret = Nothing
#else
        ret = False
#endif
     in withPlaceHolder . withEpAnnNotUsed HsDo ListComp . mkLocated . map mkLocated $
            stmts ++ [lastStmt]

-- | A type constraint on an expression.
--
-- > e :: t
-- > =====
-- > var "e" @::@ var "t"
(@::@) :: HsExpr' -> HsType' -> HsExpr'
#if MIN_VERSION_ghc(8,8,0)
e @::@ t = withEpAnnNotUsed ExprWithTySig (mkLocated e) (sigWcType t)
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
#if MIN_VERSION_ghc(9,6,0)
tyApp e t = noExt HsAppType e' noHsTok t'
#elif MIN_VERSION_ghc(9,2,0)
tyApp e t = HsAppType builtSpan e' t'
#elif MIN_VERSION_ghc(8,8,0)
tyApp e t = noExt HsAppType e' t'
#elif MIN_VERSION_ghc(8,6,0)
tyApp e t = HsAppType t' e'
#else
tyApp e t = HsAppType e' t'
#endif
  where
    t' = wcType $ unLoc $ parenthesizeTypeForApp $ mkLocated t
    e' = mkLocated e

-- | Constructs a record with explicit field names.
--
-- > A { x = y }
-- > =====
-- > recordConE "A" [("x", var "y")]
recordConE :: RdrNameStr -> [(RdrNameStr, HsExpr')] -> HsExpr'
recordConE c fs = (withPlaceHolder $ withEpAnnNotUsed RecordCon (valueRdrName c))
#if !MIN_VERSION_ghc(8,6,0)
                    noPostTcExpr
#endif
                    $ HsRecFields (map recField fs)
                        Nothing -- No ".."
  where
    recField :: (RdrNameStr, HsExpr') -> LHsRecField' LHsExpr'
    recField (f, e) =
#if MIN_VERSION_ghc(9,4,0)
        mkLocated HsFieldBind
            { hfbLHS =
                  mkLocated $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hfbRHS = mkLocated e
            , hfbPun = False
            , hfbAnn = EpAnnNotUsed
#else
        mkLocated HsRecField
            { hsRecFieldLbl =
                  builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hsRecFieldArg = mkLocated e
            , hsRecPun = False
#if MIN_VERSION_ghc(9,2,0)
            , hsRecFieldAnn = EpAnnNotUsed
#endif
#endif
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
       $ withEpAnnNotUsed RecordUpd (parenthesizeExprForApp $ mkLocated e)
       $ toRecordUpdFields $ map mkField fs
  where
    mkField :: (RdrNameStr, HsExpr') -> LHsRecUpdField'
    mkField (f, e') =
#if MIN_VERSION_ghc(9,4,0)
        mkLocated HsFieldBind
            { hfbLHS =
                mkLocated $ withPlaceHolder $ noExt Ambiguous $ valueRdrName f
            , hfbRHS = mkLocated e'
            , hfbPun = False
            , hfbAnn = EpAnnNotUsed
#else
        mkLocated HsRecField
            { hsRecFieldLbl =
                builtLoc $ withPlaceHolder $ noExt Ambiguous $ valueRdrName f
            , hsRecFieldArg = mkLocated e'
            , hsRecPun = False
#if MIN_VERSION_ghc(9,2,0)
            , hsRecFieldAnn = EpAnnNotUsed
#endif
#endif
            }
    withPlaceHolder4 = withPlaceHolder . withPlaceHolder . withPlaceHolder
                            . withPlaceHolder
#if MIN_VERSION_ghc(9,8,0)
    toRecordUpdFields = noExt RegularRecUpdFields
#elif MIN_VERSION_ghc(9,2,0)
    toRecordUpdFields = Left
#else
    toRecordUpdFields = id
#endif

arithSeq :: ArithSeqInfo GhcPs -> HsExpr'
arithSeq =
#if MIN_VERSION_ghc(8,6,0)
    withEpAnnNotUsed ArithSeq Nothing
#else
    ArithSeq noPostTcExpr Nothing
#endif

-- | An arithmetic sequence expression with a start value.
--
-- > [a ..]
-- > =====
-- > from (var "a")
from :: HsExpr' -> HsExpr'
from from' = arithSeq $ From (mkLocated from')

-- | An arithmetic sequence expression with a start and a step values.
--
-- > [a, b ..]
-- > =====
-- > fromThen (var "a") (var "b")
fromThen :: HsExpr' -> HsExpr' -> HsExpr'
fromThen from' then' = arithSeq $ FromThen (mkLocated from') (mkLocated then')

-- | An arithmetic sequence expression with a start and an end values.
--
-- > [a .. b]
-- > =====
-- > fromTo (var "a") (var "b")
fromTo :: HsExpr' -> HsExpr' -> HsExpr'
fromTo from' to = arithSeq $ FromTo (mkLocated from') (mkLocated to)

-- | An arithmetic sequence expression with a start, a step, and an end values.
--
-- > [a, b .. c]
-- > =====
-- > fromThenTo (var "a") (var "b") (var "c")
fromThenTo :: HsExpr' -> HsExpr' -> HsExpr' -> HsExpr'
fromThenTo from' then' to =
    arithSeq $ FromThenTo (mkLocated from') (mkLocated then') (mkLocated to)
