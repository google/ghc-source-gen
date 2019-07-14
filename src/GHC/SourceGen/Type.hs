-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell types.
module GHC.SourceGen.Type where

import Data.String (fromString)
import HsTypes

import GHC.SourceGen.Syntax
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Type.Internal

-- | A promoted name, for example from the @DataKinds@ extension.
tyPromotedVar :: RdrNameStr -> HsType'
tyPromotedVar = noExt HsTyVar notPromoted . typeRdrName

stringTy :: String -> HsType'
stringTy = noExt HsTyLit . noSourceText HsStrTy . fromString

numTy :: Integer -> HsType'
numTy = noExt HsTyLit . noSourceText HsNumTy

listTy :: HsType' -> HsType'
listTy = noExt HsListTy . builtLoc

listPromotedTy :: [HsType'] -> HsType'
listPromotedTy = withPlaceHolder (noExt HsExplicitListTy notPromoted) . map builtLoc

-- | A function type.
--
-- > a -> b
-- > =====
-- > var "a" --> var "b"
(-->) :: HsType' -> HsType' -> HsType'
a --> b = noExt HsFunTy (parenthesizeTypeForFun $ builtLoc a) (builtLoc b)

infixr 0 -->

forall' :: [HsTyVarBndr'] -> HsType' -> HsType'
forall' ts = noExt HsForAllTy (map builtLoc ts) . builtLoc

-- | Qualify a type with constraints.
--
-- > (F x, G x) => x
-- > =====
-- > [var "F" @@ var "x", var "G" @@ var "x"] ==> var "x"
(==>) :: [HsType'] -> HsType' -> HsType'
(==>) cs = noExt HsQualTy (builtLoc (map builtLoc cs)) . builtLoc
