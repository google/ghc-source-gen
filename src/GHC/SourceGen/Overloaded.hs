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
    , BVar(..)
    ) where

import GHC.Hs.Type
    ( HsType(..)
    , HsTyVarBndr(..)
#if MIN_VERSION_ghc(9,8,0)
    , HsBndrVis (HsBndrRequired)
#endif
    )
import GHC.Hs (IE(..), IEWrappedName(..)
#if MIN_VERSION_ghc(9,6,0)
    , noExtField
#endif
    )
#if !MIN_VERSION_ghc(8,6,0)
import PlaceHolder(PlaceHolder(..))
#endif

import GHC.Hs
    ( HsExpr(..)
    , Pat(..)
    , HsTupArg(..)
    , HsTupleSort(..)
    )
#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (Boxity(..))
import GHC.Core.DataCon (dataConName)
import GHC.Types.Name.Reader (nameRdrName)
import GHC.Builtin.Types (consDataCon_RDR, nilDataCon, unitDataCon)
import GHC.Types.Var (Specificity(..))
#else
import BasicTypes (Boxity(..))
import DataCon (dataConName)
import RdrName (nameRdrName)
import TysWiredIn (consDataCon_RDR, nilDataCon, unitDataCon)
#endif

import GHC.SourceGen.Expr.Internal
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal

-- | A class for wrapping terms in parentheses.
class Par e where
    par :: e -> e

instance Par HsExpr' where
#if MIN_VERSION_ghc(9,4,0)
    par p = withEpAnnNotUsed HsPar mkToken (mkLocated p) mkToken
#else
    par = withEpAnnNotUsed HsPar . mkLocated
#endif

instance Par Pat' where
#if MIN_VERSION_ghc(9,4,0)
    par p = withEpAnnNotUsed ParPat mkToken (builtPat p) mkToken
#else
    par = withEpAnnNotUsed ParPat . builtPat
#endif

instance Par HsType' where
    par = withEpAnnNotUsed HsParTy . mkLocated

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
        = withEpAnnNotUsed OpApp
            (parenthesizeExprForOp $ mkLocated x)
            (mkLocated $ var o)
#if !MIN_VERSION_ghc(8,6,0)
            PlaceHolder
#endif
            (parenthesizeExprForOp $ mkLocated y)
    x @@ y = withEpAnnNotUsed HsApp (parenthesizeExprForOp $ mkLocated x)
                (parenthesizeExprForApp $ mkLocated y)

instance App HsType' where
    op x o y
#if MIN_VERSION_ghc(9,4,0)
        = withEpAnnNotUsed HsOpTy notPromoted (parenthesizeTypeForOp $ mkLocated x)
                (typeRdrName o)
                (parenthesizeTypeForOp $ mkLocated y)
#else
        = noExt HsOpTy (parenthesizeTypeForOp $ mkLocated x)
                (typeRdrName o)
                (parenthesizeTypeForOp $ mkLocated y)
#endif
    x @@ y = noExt HsAppTy
                (parenthesizeTypeForOp $ mkLocated x)
                (parenthesizeTypeForApp $ mkLocated y)

class HasTuple e where
    unit :: e
    tupleOf :: Boxity -> [e] -> e

tuple, unboxedTuple :: HasTuple e => [e] -> e
tuple = tupleOf Boxed
unboxedTuple = tupleOf Unboxed

instance HasTuple HsExpr' where
    tupleOf b ts =
        explicitTuple
            (map (withEpAnnNotUsed Present . mkLocated) ts)
            b
      where
#if MIN_VERSION_ghc(9,2,0)
        explicitTuple = withEpAnnNotUsed ExplicitTuple
#else
        explicitTuple = noExt ExplicitTuple . map builtLoc
#endif
    unit = noExt HsVar unitDataConName

unitDataConName :: LIdP
unitDataConName = mkLocated $ nameRdrName $ dataConName $ unitDataCon

instance HasTuple HsType' where
    tupleOf b = withEpAnnNotUsed HsTupleTy b' . map mkLocated
        where
            b' = case b of
                    Unboxed -> HsUnboxedTuple
                    -- See the note [Unit tuples] in HsType.hs for why
                    -- this isn't just HsBoxed.
                    Boxed -> HsBoxedOrConstraintTuple
    unit = tupleOf Boxed []

instance HasTuple Pat' where
    tupleOf b ps =
        withEpAnnNotUsed TuplePat (map builtPat ps) b
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

nilDataConName :: LIdP
nilDataConName = mkLocated $ nameRdrName $ dataConName $ nilDataCon

instance HasList HsExpr' where
    list = withPlaceHolder (withEpAnnNotUsed explicitList) . map mkLocated
      where
#if MIN_VERSION_ghc(9,2,0)
        explicitList = ExplicitList
#else
        explicitList x = ExplicitList x Nothing
#endif
    nil = noExt HsVar nilDataConName
    cons = noExt HsVar $ mkLocated consDataCon_RDR

instance HasList Pat' where
#if MIN_VERSION_ghc(8,6,0)
    list = withEpAnnNotUsed ListPat . map builtPat
#else
    list ps = ListPat (map builtPat ps) PlaceHolder Nothing
#endif
    nil = noExt VarPat nilDataConName
    cons = noExt VarPat $ mkLocated $ consDataCon_RDR

-- | Terms that can contain references to locally-bound variables.
--
-- Depending on the context, @'bvar' \"a\"@ could refer to either a
-- pattern variable or a type variable.
class BVar a where
    bvar :: OccNameStr -> a

-- | Terms that can contain references to named things.  They may be actual variables,
-- functions, or constructors.  For example, @'var' \"a\"@ and @'var' \"A\"@
-- are equally valid.
-- Depending on the context, the former could refer to either a function,
-- value, type variable, or pattern; and the latter could refer to either a type
-- constructor or a  data constructor,
class BVar a => Var a where
    var :: RdrNameStr -> a

instance BVar Pat' where
    bvar = noExt VarPat . valueRdrName . UnqualStr

instance Var HsExpr' where
    var = noExt HsVar . valueRdrName

instance BVar HsExpr' where
    bvar = var . UnqualStr

instance Var HsType' where
    var = withEpAnnNotUsed HsTyVar notPromoted . typeRdrName

instance BVar HsType' where
    bvar = var . UnqualStr

#if MIN_VERSION_ghc(9,0,0)
instance BVar HsTyVarBndr' where
#if MIN_VERSION_ghc(9,8,0)
    bvar = withEpAnnNotUsed UserTyVar HsBndrRequired . typeRdrName . UnqualStr
#else
    bvar = withEpAnnNotUsed UserTyVar () . typeRdrName . UnqualStr
#endif
instance BVar HsTyVarBndrS' where
    bvar = withEpAnnNotUsed UserTyVar SpecifiedSpec . typeRdrName . UnqualStr
#else
instance BVar HsTyVarBndr' where
    bvar = withEpAnnNotUsed UserTyVar . typeRdrName . UnqualStr
#endif

instance Var IE' where
    var n = ie_var $ mkLocated $ ie_name $ exportRdrName n
      where
        ie_var =
#if MIN_VERSION_ghc(9,8,0)
          IEVar Nothing
#else
          noExt IEVar
#endif

        ie_name = 
#if MIN_VERSION_ghc(9,6,0)
          noExt IEName
#else
          IEName
#endif

instance BVar IE' where
    bvar = var . UnqualStr
