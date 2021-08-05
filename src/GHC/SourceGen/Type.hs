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

import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Type.Internal

#if MIN_VERSION_ghc(9,0,1)
import GHC.Hs.Type
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
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
a --> b =
    noExt HsFunTy
#if MIN_VERSION_ghc(9,0,1)
        (HsUnrestrictedArrow NormalSyntax)
#endif
        (parenthesizeTypeForFun $ builtLoc a)
        (builtLoc b)

infixr 0 -->

-- | A type variable binding.
--
-- > forall a . T a
-- > =====
-- > forall' [bvar "a"] $ var "T" @@ var "a"
forall' :: [HsTyVarBndrSpec'] -> HsType' -> HsType'
forall' ts =
    noExt HsForAllTy
#if MIN_VERSION_ghc(9,0,1)
        (noExt HsForAllInvis (map builtLoc ts)) . builtLoc
#else
#if     MIN_VERSION_ghc(8,10,0)
        ForallInvis  -- "Invisible" forall, i.e., with a dot
#endif
        (map builtLoc ts) . builtLoc
#endif

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
kindedVar :: OccNameStr -> HsType' -> HsTyVarBndrUnit' 
kindedVar v t =
    noExt KindedTyVar
#if MIN_VERSION_ghc(9,0,1)
    ()
#endif
    (typeRdrName $  UnqualStr v)
    (builtLoc t)
