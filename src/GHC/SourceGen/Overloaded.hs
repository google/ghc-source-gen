-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
-- | This module overloads some combinators so they can be used in
-- different contexts: for expressions, types and/or patterns.
module GHC.SourceGen.Overloaded
    ( Par(..)
    , App(..)
    , HasTuple(..)
    , tuple
    , unboxedTuple
    , HasList(..)
    , Var(..)
    ) where

import BasicTypes (Boxity(..))
import HsTypes
    ( HsType(..)
    , HsTyVarBndr(..)
    )
import HsSyn (IE(..), IEWrappedName(..))
#if !MIN_VERSION_ghc(8,6,0)
import PlaceHolder(PlaceHolder(..))
#endif

import HsSyn
    ( HsExpr(..)
    , Pat(..)
    , HsTupArg(..)
    , HsTupleSort(..)
    )
import DataCon (dataConName)
import RdrName (RdrName, nameRdrName)
import SrcLoc (Located)
import TysWiredIn (consDataCon_RDR, nilDataCon, unitDataCon)

import GHC.SourceGen.Expr.Internal
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal

-- | A class for wrapping terms in parentheses.
class Par e where
    par :: e -> e

instance Par HsExpr' where
    par = noExt HsPar . builtLoc

instance Par Pat' where
    par = noExt ParPat . builtPat

instance Par HsType' where
    par = noExt HsParTy . builtLoc

-- | A class for term application.
--
-- These functions may add additional parentheses to the AST.
-- GHC's pretty-printing functions expect those parentheses
-- to already be present, because GHC preserves parentheses when it
-- parses the AST from a source file.
class App e where
    -- | Prefix-apply a term:
    --
    -- > f x
    -- > =====
    -- > var "f" @@ var "x"
    --
    -- > (+) x
    -- > =====
    -- > var "+" @@ var "x"
    --
    -- Also parenthesizes the right-hand side in order to preserve its
    -- semantics when pretty-printed, but tries to do so only when
    -- necessary:
    --
    -- > f x y
    -- > =====
    -- > var "f" @@ var "x" @@ var "y"
    -- > -- equivalently:
    -- > (var "f" @@ var "x") @@ var "y"
    --
    -- > f (g x)
    -- > =====
    -- > var "f" @@ (var "g" @@ var "x")
    --
    -- > f (g x)
    -- > =====
    -- > var "f" @@ par (var "g" @@ par (var "x"))
    (@@) :: e -> e -> e

    -- | Infix-apply an operator or function.
    --
    -- For example:
    --
    -- > x + y
    -- > =====
    -- > op (var "x") "+" (var "y")
    --
    -- Also parenthesizes the right-hand side in order to preserve its
    -- semantics when pretty-printed, but tries to do so only when necessary:
    --
    -- > f x + g y
    -- > =====
    -- > op (var "f" @@ var "x") "+" (var "g" @@ var "y")
    --
    -- > x + (y + z)
    -- > =====
    -- > op (var "x") "+" (op (var "y") "+" (var "z"))
    --
    -- > f x `plus` g y
    -- > =====
    -- > op (var "f" @@ var "x") "plus" (var "g" @@ var "y")
    op :: e -> RdrNameStr -> e -> e
infixl 2 @@

instance App HsExpr' where
    op x o y
        = noExt OpApp
            (parenthesizeExprForOp $ builtLoc x)
            (builtLoc $ var o)
#if !MIN_VERSION_ghc(8,6,0)
            PlaceHolder
#endif
            (parenthesizeExprForOp $ builtLoc y)
    x @@ y = noExt HsApp (parenthesizeExprForOp $ builtLoc x)
                (parenthesizeExprForApp $ builtLoc y)

instance App HsType' where
    op x o y
        = noExt HsOpTy (parenthesizeTypeForOp $ builtLoc x)
                (typeRdrName o)
                (parenthesizeTypeForOp $ builtLoc y)
    x @@ y = noExt HsAppTy
                (parenthesizeTypeForOp $ builtLoc x)
                (parenthesizeTypeForApp $ builtLoc y)

class HasTuple e where
    unit :: e
    tupleOf :: Boxity -> [e] -> e

tuple, unboxedTuple :: HasTuple e => [e] -> e
tuple = tupleOf Boxed
unboxedTuple = tupleOf Unboxed

instance HasTuple HsExpr' where
    tupleOf b ts =
        noExt ExplicitTuple
            (map (builtLoc . noExt Present . builtLoc) ts)
            b
    unit = noExt HsVar unitDataConName

unitDataConName :: Located RdrName
unitDataConName = builtLoc $ nameRdrName $ dataConName $ unitDataCon

instance HasTuple HsType' where
    tupleOf b = noExt HsTupleTy b' . map builtLoc
        where
            b' = case b of
                    Unboxed -> HsUnboxedTuple
                    -- See the note [Unit tuples] in HsType.hs for why
                    -- this isn't just HsBoxed.
                    Boxed -> HsBoxedOrConstraintTuple
    unit = tupleOf Boxed []

instance HasTuple Pat' where
    tupleOf b ps =
        noExt TuplePat (map builtPat ps) b
#if !MIN_VERSION_ghc(8,6,0)
        []
#endif
    unit = noExt VarPat unitDataConName

-- | An explicit list of terms.
--
-- > [x, y]
-- > =====
-- > list [var "x", var "y"]
--
-- NOTE: for types, use either @listTy@ or @promotedListTy@.
class HasList e where
    list :: [e] -> e
    -- | The empty list @[]@.
    nil :: e

    -- | The list cons constructor @(:)@.
    cons :: e

-- TODO: allow something like "consOp" which applies (:) as an operator, but using
-- the built-in RdrName.

nilDataConName :: Located RdrName
nilDataConName = builtLoc $ nameRdrName $ dataConName $ nilDataCon

instance HasList HsExpr' where
    list = withPlaceHolder (noExt ExplicitList) Nothing . map builtLoc
    nil = noExt HsVar nilDataConName
    cons = noExt HsVar $ builtLoc consDataCon_RDR

instance HasList Pat' where
#if MIN_VERSION_ghc(8,6,0)
    list = noExt ListPat . map builtPat
#else
    list ps = ListPat (map builtPat ps) PlaceHolder Nothing
#endif
    nil = noExt VarPat nilDataConName
    cons = noExt VarPat $ builtLoc $ consDataCon_RDR

-- | Terms that can contain references to named things.  They may be actual variables,
-- functions, or constructors.  For example, @'var' \"a\"@ and @'var' \"A\"@
-- are equally valid.
-- Depending on the context, the former could refer to either a function,
-- value, type variable, or pattern; and the latter could refer to either a type
-- constructor or a  data constructor,
class Var a where
    var :: RdrNameStr -> a

instance Var Pat' where
    var = noExt VarPat . valueRdrName

instance Var HsExpr' where
    var = noExt HsVar . valueRdrName

instance Var HsType' where
    var = noExt HsTyVar notPromoted . typeRdrName

instance Var HsTyVarBndr' where
    var = noExt UserTyVar . typeRdrName

instance Var IE' where
    var n = noExt IEVar $ builtLoc $ IEName $ exportRdrName n
