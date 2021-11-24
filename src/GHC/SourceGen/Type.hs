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
#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs.Type
import GHC.Parser.Annotation
#else
import GHC.Hs.Type
#endif

import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Type.Internal

-- | A promoted name, for example from the @DataKinds@ extension.
tyPromotedVar :: RdrNameStr -> HsType'
tyPromotedVar = withEpAnnNotUsed HsTyVar promoted . typeRdrName

stringTy :: String -> HsType'
stringTy = noExt HsTyLit . noSourceText HsStrTy . fromString

numTy :: Integer -> HsType'
numTy = noExt HsTyLit . noSourceText HsNumTy

listTy :: HsType' -> HsType'
listTy = withEpAnnNotUsed HsListTy . mkLocated

listPromotedTy :: [HsType'] -> HsType'
-- Lists of two or more elements don't need the explicit tick (`'`).
-- But for consistency, just always add it.
listPromotedTy = withPlaceHolder (withEpAnnNotUsed HsExplicitListTy promoted) . map mkLocated

tuplePromotedTy :: [HsType'] -> HsType'
tuplePromotedTy = withPlaceHolders (withEpAnnNotUsed HsExplicitTupleTy) . map mkLocated

-- | A function type.
--
-- > a -> b
-- > =====
-- > var "a" --> var "b"
(-->) :: HsType' -> HsType' -> HsType'
a --> b = withEpAnnNotUsed HsFunTy
#if MIN_VERSION_ghc(9,0,0)
         (HsUnrestrictedArrow NormalSyntax)
#endif
         (parenthesizeTypeForFun $ mkLocated a) (mkLocated b)

infixr 0 -->

-- | A type variable binding.
--
-- > forall a . T a
-- > =====
-- > forall' [bvar "a"] $ var "T" @@ var "a"
forall' :: [HsTyVarBndrS'] -> HsType' -> HsType'
forall' ts = noExt hsForAllTy (map mkLocated ts) . mkLocated
  where
#if MIN_VERSION_ghc(9,2,0)
    hsForAllTy x = HsForAllTy x . withEpAnnNotUsed mkHsForAllInvisTele
#elif MIN_VERSION_ghc(9,0,0)
    hsForAllTy x = HsForAllTy x . mkHsForAllInvisTele
#elif MIN_VERSION_ghc(8,10,0)
    fvf = ForallInvis -- "Invisible" forall, i.e., with a dot
    hsForAllTy x = HsForAllTy x fvf
#else
    hsForAllTy = HsForAllTy
#endif

-- | Qualify a type with constraints.
--
-- > (F x, G x) => x
-- > =====
-- > [var "F" @@ var "x", var "G" @@ var "x"] ==> var "x"
(==>) :: [HsType'] -> HsType' -> HsType'
(==>) cs = hsQualTy (mkLocated (map mkLocated cs)) . mkLocated
  where
#if MIN_VERSION_ghc(9,2,0)
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
kindedVar v t = withEpAnnNotUsed KindedTyVar
#if MIN_VERSION_ghc(9,0,0)
                ()
#endif
                (typeRdrName $ UnqualStr v) (mkLocated t)
