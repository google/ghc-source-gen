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
    , (==>)
    , kindedVar
    ) where

import Data.String (fromString)
import GHC.Hs.Type

import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Type.Internal
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Types.Var (Specificity(SpecifiedSpec))

import GHC.Hs.Extension (GhcPs)

#if MIN_VERSION_ghc(9,0,1)
import GHC.Hs.Type
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Types.Var (Specificity(SpecifiedSpec))
#else
import GHC.Hs.Types
#endif

-- | A promoted name, for example from the @DataKinds@ extension.
tyPromotedVar :: RdrNameStr -> HsType'
tyPromotedVar = noExt HsTyVar promoted . typeRdrName

stringTy :: String -> HsType'
stringTy = noExt HsTyLit . noSourceText HsStrTy . fromString

numTy :: Integer -> HsType'
numTy = noExt HsTyLit . noSourceText HsNumTy

listTy :: HsType' -> HsType'
listTy = noExt HsListTy . builtLoc

listPromotedTy :: [HsType'] -> HsType'
-- Lists of two or more elements don't need the explicit tick (`'`).
-- But for consistency, just always add it.
listPromotedTy = withPlaceHolder (noExt HsExplicitListTy promoted) . map builtLoc

tuplePromotedTy :: [HsType'] -> HsType'
tuplePromotedTy = withPlaceHolders (noExt HsExplicitTupleTy) . map builtLoc

-- | A function type.
--
-- > a -> b
-- > =====
-- > var "a" --> var "b"
(-->) :: HsType' -> HsType' -> HsType'
a --> b = noExt HsFunTy (HsUnrestrictedArrow NormalSyntax) (parenthesizeTypeForFun $ builtLoc a) (builtLoc b)

infixr 0 -->

-- | A type variable binding.
--
-- > forall a . T a
-- > =====
-- > forall' [bvar "a"] $ var "T" @@ var "a"
forall' :: [HsTyVarBndr Specificity GhcPs] -> HsType' -> HsType'
forall' ts = noExt HsForAllTy (noExt HsForAllInvis (map builtLoc ts)) . builtLoc

-- | Qualify a type with constraints.
--
-- > (F x, G x) => x
-- > =====
-- > [var "F" @@ var "x", var "G" @@ var "x"] ==> var "x"
(==>) :: [HsType'] -> HsType' -> HsType'
(==>) cs = noExt HsQualTy (builtLoc (map builtLoc cs)) . builtLoc

infixr 0 ==>

-- | A type variable with a kind signature.
--
-- > x :: A
-- > =====
-- > kindedVar "x" (var "A")
kindedVar :: OccNameStr -> HsType' -> HsTyVarBndr Specificity GhcPs
kindedVar v t = noExt KindedTyVar SpecifiedSpec (typeRdrName $  UnqualStr v)
                        (builtLoc t)
