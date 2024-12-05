{-# LANGUAGE CPP #-}
-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell types.
module GHC.SourceGen.Type
    ( HsType'
    , tyPromotedVar
    , stringTy
    , numTy
    , listTy
    , listPromotedTy
    , tuplePromotedTy
    , (-->)
    , forall'
    , HsTyVarBndr'
    , (==>)
    , kindedVar
    ) where

import Data.String (fromString)
import GHC.Hs.Type
import GHC.Parser.Annotation

#if MIN_VERSION_ghc(9,4,0)
import Language.Haskell.Syntax.Extension
#endif

import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Type.Internal

-- | A promoted name, for example from the @DataKinds@ extension.
tyPromotedVar :: RdrNameStr -> HsType'
#if MIN_VERSION_ghc(9,10,0)
tyPromotedVar = HsTyVar [] promoted . typeRdrName
#else
tyPromotedVar = withEpAnnNotUsed HsTyVar promoted . typeRdrName
#endif

stringTy :: String -> HsType'
stringTy = noExt HsTyLit . noSourceText HsStrTy . fromString

numTy :: Integer -> HsType'
numTy = noExt HsTyLit . noSourceText HsNumTy

listTy :: HsType' -> HsType'
#if MIN_VERSION_ghc(9,10,0)
listTy = HsListTy (AnnParen AnnParens noSpanAnchor noSpanAnchor) . mkLocated
#else
listTy = withEpAnnNotUsed HsListTy . mkLocated
#endif

listPromotedTy :: [HsType'] -> HsType'
-- Lists of two or more elements don't need the explicit tick (`'`).
-- But for consistency, just always add it.
#if MIN_VERSION_ghc(9,10,0)
listPromotedTy = withPlaceHolder (HsExplicitListTy [] promoted) . map mkLocated
#else
listPromotedTy = withPlaceHolder (withEpAnnNotUsed HsExplicitListTy promoted) . map mkLocated
#endif

tuplePromotedTy :: [HsType'] -> HsType'
#if MIN_VERSION_ghc(9,10,0)
tuplePromotedTy = withPlaceHolders (withEpAnnNotUsed (HsExplicitTupleTy [])) . map mkLocated
#else
tuplePromotedTy = withPlaceHolders (withEpAnnNotUsed HsExplicitTupleTy) . map mkLocated
#endif

-- | A function type.
--
-- > a -> b
-- > =====
-- > var "a" --> var "b"
(-->) :: HsType' -> HsType' -> HsType'
a --> b =
#if MIN_VERSION_ghc(9,10,0)
     (noExt HsFunTy)
         (HsUnrestrictedArrow (EpUniTok noSpanAnchor NormalSyntax))
         (parenthesizeTypeForFun $ mkLocated a) (mkLocated b)
#elif MIN_VERSION_ghc(9,4,0)
     withEpAnnNotUsed HsFunTy
         (HsUnrestrictedArrow mkUniToken)
         (parenthesizeTypeForFun $ mkLocated a) (mkLocated b)
#else
     withEpAnnNotUsed HsFunTy
         (HsUnrestrictedArrow NormalSyntax)
         (parenthesizeTypeForFun $ mkLocated a) (mkLocated b)
#endif

infixr 0 -->

-- | A type variable binding.
--
-- > forall a . T a
-- > =====
-- > forall' [bvar "a"] $ var "T" @@ var "a"
forall' :: [HsTyVarBndrS'] -> HsType' -> HsType'
#if MIN_VERSION_ghc(9,10,0)
forall' ts t =
    HsForAllTy
        NoExtField
        (mkHsForAllInvisTele ann (map mkLocated ts))
        (mkLocated t)
  where
    ann = EpAnn noSpanAnchor (noAnn, noAnn) emptyComments
#elif MIN_VERSION_ghc(9,2,0)
forall' ts = noExt hsForAllTy (map mkLocated ts) . mkLocated
  where
    hsForAllTy x = HsForAllTy x . withEpAnnNotUsed mkHsForAllInvisTele
#else
forall' ts = noExt hsForAllTy (map mkLocated ts) . mkLocated
  where
    hsForAllTy x = HsForAllTy x . mkHsForAllInvisTele
#endif

-- | Qualify a type with constraints.
--
-- > (F x, G x) => x
-- > =====
-- > [var "F" @@ var "x", var "G" @@ var "x"] ==> var "x"
(==>) :: [HsType'] -> HsType' -> HsType'
(==>) cs = hsQualTy (mkLocated (map mkLocated cs)) . mkLocated
  where
#if MIN_VERSION_ghc(9,4,0)
    hsQualTy = noExt HsQualTy
#elif MIN_VERSION_ghc(9,2,0)
    hsQualTy = noExt HsQualTy . Just
#else
    hsQualTy = noExt HsQualTy
#endif

infixr 0 ==>

-- | A type variable with a kind signature.
--
-- > x :: A
-- > =====
-- > kindedVar "x" (var "A")
kindedVar :: OccNameStr -> HsType' -> HsTyVarBndr'
kindedVar v t =
#if MIN_VERSION_ghc(9,10,0)
            KindedTyVar
                []
                (noExt HsBndrRequired)
                (typeRdrName $ UnqualStr v) (mkLocated t)
#elif MIN_VERSION_ghc(9,8,0)
            withEpAnnNotUsed KindedTyVar
                HsBndrRequired
                (typeRdrName $ UnqualStr v) (mkLocated t)
#else
            withEpAnnNotUsed KindedTyVar
                ()
                (typeRdrName $ UnqualStr v) (mkLocated t)
#endif
