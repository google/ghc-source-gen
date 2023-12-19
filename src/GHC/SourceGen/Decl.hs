-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides combinators for constructing Haskell declarations.
module GHC.SourceGen.Decl
    ( HsDecl'
      -- * Type declarations
    , type'
    , newtype'
    , data'
      -- ** Data constructors
    , ConDecl'
    , prefixCon
    , infixCon
    , recordCon
    , Field
    , field
    , strict
    , lazy
      -- ** Deriving clauses
    , HsDerivingClause'
    , deriving'
    , derivingStock
    , derivingAnyclass
    , derivingNewtype
#if MIN_VERSION_ghc(8,6,0)
    , derivingVia
#endif
    , standaloneDeriving
    , standaloneDerivingStock
    , standaloneDerivingNewtype
    , standaloneDerivingAnyclass
      -- * Class declarations
    , class'
    , ClassDecl
    , funDep
      -- * Instance declarations
    , instance'
    , RawInstDecl
    , HasTyFamInst(..)
    , tyFamInst
      -- * Pattern synonyms
    , patSynSigs
    , patSynSig
    , patSynBind
    ) where

#if MIN_VERSION_ghc(9,0,0)
import GHC (LexicalFixity(Prefix))
import GHC.Data.Bag (listToBag)

#if MIN_VERSION_ghc(9,6,0)
import GHC (GhcPs, LayoutInfo (NoLayoutInfo))
#else
import GHC.Types.SrcLoc (LayoutInfo(NoLayoutInfo))
#endif

#else
import BasicTypes (LexicalFixity(Prefix))
import Bag (listToBag)
#endif
#if !MIN_VERSION_ghc(8,6,0)
import BasicTypes (DerivStrategy(..))
#endif
import GHC.Hs.Binds
import GHC.Hs.Decls

import GHC.Hs.Type
    ( ConDeclField(..)
    , FieldOcc(..)
    , HsConDetails(..)
#if !MIN_VERSION_ghc(9,2,0)
    , HsImplicitBndrs (..)
#endif
#if MIN_VERSION_ghc(9,2,0)
    , HsOuterTyVarBndrs (..)
#endif
    , HsSrcBang(..)
    , HsType(..)
    , LHsType
#if MIN_VERSION_ghc(8,6,0)
    , HsWildCardBndrs (..)
#endif
#if MIN_VERSION_ghc(8,8,0)
    , HsArg(..)
#endif
    , SrcStrictness(..)
    , SrcUnpackedness(..)
#if MIN_VERSION_ghc(9,0,0)
    , hsUnrestricted
#endif
    )

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation (AnnSortKey(..), EpAnn(..))
#elif MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Extension (NoExtField(NoExtField))
#elif MIN_VERSION_ghc(8,6,0)
import GHC.Hs.Extension (NoExt(NoExt))
#else
import PlaceHolder (PlaceHolder(..))
#endif

import GHC.SourceGen.Binds.Internal
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal
import GHC.Hs

-- | A definition that can appear in the body of a @class@ declaration.
--
-- 'ClassDecl' definitions may be constructed using 'funDep' or using the
-- instance of 'HasValBind'.  For more details, see the documentation of
-- that function, and of "GHC.SourceGen.Binds" overall.
data ClassDecl
    = ClassSig Sig'
    | ClassDefaultMethod HsBind'
    | ClassFunDep [RdrNameStr] [RdrNameStr]
    -- TODO: type families

instance HasValBind ClassDecl where
    sigB = ClassSig
    bindB = ClassDefaultMethod

-- | A functional dependency for a class.
--
-- > | a, b -> c
-- > =====
-- > funDep ["a", "b"] ["c"]
--
-- > class Ident a b | a -> b, b -> a where
-- >   ident :: a -> b
-- > =====
-- > class' [] "Ident" ["a", "b"]
-- >    [ funDep ["a"] ["b"]
-- >    , funDep ["b"] ["a"]
-- >    , typeSig "ident" $ var "a" --> var "b"
-- >    ]
funDep :: [RdrNameStr] -> [RdrNameStr] -> ClassDecl
funDep = ClassFunDep

-- TODO:
-- - kinded variables
-- - fixity of declaration
-- - functional dependencies
-- - associated types

-- | A class declaration.
--
-- > class (Real a, Enum a) => Integral a where
-- >   divMod :: a -> a -> (a, a)
-- >   div :: a -> a -> a
-- >   div x y = fst (divMod x y)
-- > =====
-- > let a = var "a"
-- > in class'
-- >      [var "Real" @@ a, var "Enum" @@ a]
-- >      "Integral"
-- >      [bvar "a"]
-- >      [ typeSig "divMod" $ a --> a --> tuple [a, a]
-- >      , typeSig "div" $ a --> a --> a
-- >      , funBind "div"
-- >          $ match [bvar "x", bvar "y"]
-- >             $ var "fst" @@ (var "divMod" @@ var "x" @@ var "y")
-- >      ]
class'
    :: [HsType'] -- ^ Context
    -> OccNameStr -- ^ Class name
    -> [HsTyVarBndr'] -- ^ Type parameters
    -> [ClassDecl] -- ^ Class declarations
    -> HsDecl'
class' context name vars decls
    = noExt TyClD $ ClassDecl
            { tcdCtxt = toHsContext $ mkLocated $ map mkLocated context
#if MIN_VERSION_ghc(9,6,0)
            , tcdLayout = NoLayoutInfo
            , tcdCExt = (EpAnnNotUsed, NoAnnSortKey)
#elif MIN_VERSION_ghc(9,2,0)
            , tcdCExt = (EpAnnNotUsed, NoAnnSortKey, NoLayoutInfo)
#elif MIN_VERSION_ghc(9,0,0)
            , tcdCExt = NoLayoutInfo
#elif MIN_VERSION_ghc(8,10,0)
            , tcdCExt = NoExtField
#elif MIN_VERSION_ghc(8,6,0)
            , tcdCExt = NoExt
#else
            , tcdFVs = PlaceHolder
#endif
            , tcdLName = typeRdrName $ unqual name
            , tcdTyVars = mkQTyVars vars
            , tcdFixity = Prefix
            , tcdFDs = [ mkLocated $ funDep' (map typeRdrName xs) (map typeRdrName ys)
                       | ClassFunDep xs ys <- decls
                       ]
            , tcdSigs = [mkLocated sig | ClassSig sig <- decls]
            , tcdMeths =
                listToBag [mkLocated bind | ClassDefaultMethod bind <- decls]
            , tcdATs = []  -- Associated types
            , tcdATDefs = []  -- Associated type defaults
            , tcdDocs = []  -- Haddocks
            }
  where
#if MIN_VERSION_ghc(9,2,0)
    funDep' = withEpAnnNotUsed FunDep
#else
    funDep' = (,)
#endif
#if MIN_VERSION_ghc(9,2,0)
    toHsContext = Just
#else
    toHsContext = id
#endif

-- | A definition that can appear in the body of an @instance@ declaration.
--
-- 'RawInstDecl' definitions may be constructed using its class instances, e.g.,
-- 'HasValBind'.  For more details, see the documentation of those classes.
data RawInstDecl
    = InstSig Sig'
    | InstBind HsBind'
    | InstTyFam TyFamInstDecl'

instance HasValBind RawInstDecl where
    sigB = InstSig
    bindB = InstBind

-- | An instance declaration.
--
-- > instance Show Bool where
-- >   show :: Bool -> String -- Requires the InstanceSigs extension
-- >   show True = "True"
-- >   show False = "False"
-- > =====
-- > instance' (var "Show" @@ var "Bool")
-- >   [ typeSig "show" $ var "Bool" --> var "String"
-- >   , funBinds "show"
-- >       [ match [bvar "True"] $ string "True"
-- >       , match [bvar "False"] $ string "False"
-- >       ]
-- >   ]
instance' :: HsType' -> [RawInstDecl] -> HsDecl'
instance' ty decls = noExt InstD  $ noExt ClsInstD $ ClsInstDecl
    { cid_poly_ty = sigType ty
#if MIN_VERSION_ghc(9,2,0)
    , cid_ext = (EpAnnNotUsed, NoAnnSortKey)
#elif MIN_VERSION_ghc(8,10,0)
    , cid_ext = NoExtField
#elif MIN_VERSION_ghc(8,6,0)
    , cid_ext = NoExt
#endif
    , cid_binds = listToBag [mkLocated b | InstBind b <- decls]
    , cid_sigs = [mkLocated sig | InstSig sig <- decls]
    , cid_tyfam_insts = [mkLocated $ t | InstTyFam t <- decls]
    , cid_datafam_insts = []
    , cid_overlap_mode = Nothing
    }

-- | Terms which can contain a type instance declaration.
--
-- To use this class, call 'tyFamInst'.
class HasTyFamInst t where
    tyFamInstD :: TyFamInstDecl' -> t

instance HasTyFamInst HsDecl' where
    tyFamInstD = noExt InstD . noExt TyFamInstD

instance HasTyFamInst RawInstDecl where
    tyFamInstD = InstTyFam

-- | A type family instance.
--
-- > type Elt String = Char
-- > =====
-- > tyFamInst "Elt" [var "String"] (var "Char")
tyFamInst :: HasTyFamInst t => RdrNameStr -> [HsType'] -> HsType' -> t
tyFamInst name params ty = tyFamInstD
        $ tyFamInstDecl
        $ famEqn
            (typeRdrName name)
            eqn_bndrs
            (map mkLocated params)
            Prefix
            (mkLocated ty)
  where
#if MIN_VERSION_ghc(9,2,0)
    tyFamInstDecl = withEpAnnNotUsed TyFamInstDecl
#else
    tyFamInstDecl = TyFamInstDecl . withPlaceHolder . noExt (withPlaceHolder HsIB)
#endif
#if MIN_VERSION_ghc(9,2,0)
    famEqn tycon bndrs pats = withEpAnnNotUsed FamEqn tycon bndrs (map HsValArg pats)
#elif MIN_VERSION_ghc(8,8,0)
    famEqn tycon bndrs pats = noExt FamEqn tycon bndrs (map HsValArg pats)
#else
    famEqn tycon _ = noExt FamEqn tycon
#endif
#if MIN_VERSION_ghc(9,2,0)
    eqn_bndrs = noExt HsOuterImplicit
#else
    eqn_bndrs = Nothing
#endif

-- | Declares a type synonym.
--
-- > type A a b = B b a
-- > =====
-- > type' "A" [bvar "a", bvar "b"] $ var "B" @@ var "b" @@ var "a"
type' :: OccNameStr -> [HsTyVarBndr'] -> HsType' -> HsDecl'
type' name vars t =
    noExt TyClD $ withPlaceHolder $ withEpAnnNotUsed SynDecl (typeRdrName $ unqual name)
        (mkQTyVars vars)
        Prefix
        (mkLocated t)

newOrDataType
    :: NewOrData
    -> OccNameStr
    -> [HsTyVarBndr']
    -> [ConDecl']
    -> [HsDerivingClause']
    -> HsDecl'
newOrDataType newOrData name vars conDecls derivs
    = noExt TyClD $ withPlaceHolder $ withPlaceHolder $
        withEpAnnNotUsed DataDecl (typeRdrName $ unqual name)
            (mkQTyVars vars)
            Prefix
            $ noExt HsDataDefn
#if !MIN_VERSION_ghc(9,6,0)
                newOrData
#endif
                cxt
                Nothing
                Nothing
#if MIN_VERSION_ghc(9,6,0)
                (case newOrData of
                    NewType -> case conDecls of
                        [decl] -> NewTypeCon $ mkLocated decl
                        _ -> error "NewTypeCon with more than one decl"
                    DataType -> DataTypeCons False (map mkLocated conDecls)
                )  
#else
                (map mkLocated conDecls)
#endif
#if MIN_VERSION_ghc(9,4,0)
                (toHsDeriving $ map mkLocated derivs)
#else
                (toHsDeriving $ map builtLoc derivs)
#endif
  where
#if MIN_VERSION_ghc(9,2,0)
    cxt = Nothing
#else
    cxt = builtLoc []
#endif
#if MIN_VERSION_ghc(9,2,0)
    toHsDeriving = id
#else
    toHsDeriving = mkLocated
#endif

-- | A newtype declaration.
--
-- > newtype Const a b = Const a deriving Eq
-- > =====
-- > newtype' "Const" [bvar "a", bvar "b"]
-- >    (conDecl "Const" [var "a"])
-- >    [var "Show"]
newtype' :: OccNameStr -> [HsTyVarBndr'] -> ConDecl' -> [HsDerivingClause'] -> HsDecl'
newtype' name vars conD = newOrDataType NewType name vars [conD]

-- | A data declaration.
--
-- > data Either a b = Left a | Right b
-- >    deriving Show
-- > =====
-- > data' "Either" [bvar "a", bvar "b"]
-- >   [ conDecl "Left" [var "a"]
-- >   , conDecl "Right" [var "b"]
-- >   ]
-- >   [var "Show"]
data' :: OccNameStr -> [HsTyVarBndr'] -> [ConDecl'] -> [HsDerivingClause'] -> HsDecl'
data' = newOrDataType DataType

-- | Declares a Haskell-98-style prefix constructor for a data or type
-- declaration.
--
-- > Foo a Int
-- > =====
-- > prefixCon "Foo" [field (var "a"), field (var "Int")]
prefixCon :: OccNameStr -> [Field] -> ConDecl'
prefixCon name fields = renderCon98Decl name
    $ prefixCon' $ map (hsUnrestricted . renderField) fields
  where
#if MIN_VERSION_ghc(9,2,0)
    prefixCon' = PrefixCon []
#else
    prefixCon' = PrefixCon
#endif

-- | Declares a Haskell-98-style infix constructor for a data or type
-- declaration.
--
-- > A b :+: C d
-- > =====
-- > infixCon (field (var "A" @@ var "b")) ":+:" (field (Var "C" @@ var "d"))
infixCon :: Field -> OccNameStr -> Field -> ConDecl'
infixCon f name f' = renderCon98Decl name
    $ InfixCon (hsUnrestricted $ renderField f) (hsUnrestricted $ renderField f')

-- | Declares Haskell-98-style record constructor for a data or type
-- declaration.
--
-- > A { x :: B, y :: C }
-- > =====
-- > recordCon "A" [("x", var "B"), ("y", var "C")]
recordCon :: OccNameStr -> [(OccNameStr, Field)] -> ConDecl'
recordCon name fields = renderCon98Decl name
    $ RecCon $ mkLocated $ map mkLConDeclField fields
  where
    mkLConDeclField (n, f) =
        mkLocated $ withEpAnnNotUsed ConDeclField
#if MIN_VERSION_ghc(9,4,0)
                        [mkLocated $ withPlaceHolder $ noExt FieldOcc $ valueRdrName $ unqual n]
#else
                        [builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName $ unqual n]
#endif
                        (renderField f)
                        Nothing

-- | An individual argument of a data constructor.  Contains a type for the field,
-- and whether the field is strict or lazy.
data Field = Field
    { fieldType :: HsType'
    , strictness :: SrcStrictness
    }

-- | A field with no explicit strictness annotations.
--
-- > A b
-- > =====
-- > field $ var "A" @@ var "b"
field :: HsType' -> Field
field t = Field t NoSrcStrict

-- | Give a field an explicit strictness annotation.  Overrides any such previous
-- annotations (for example, from 'lazy').
--
-- > !(A b)
-- > =====
-- > strict $ field $ var "A" @@ var "b"
strict :: Field -> Field
strict f = f { strictness = SrcStrict }

-- | Give a field an explicit laziness annotation.  This feature is useful in combination
-- with the @StrictData@ extension.  Overrides any such previous
-- annotations (for example, from 'strict').
--
-- > !(A b)
-- > =====
-- > strict $ field $ var "A" @@ var "b"
lazy :: Field -> Field
lazy f = f { strictness = SrcLazy }

#if !MIN_VERSION_ghc(9,0,0)
hsUnrestricted :: a -> a
hsUnrestricted = id
#endif

renderField :: Field -> LHsType GhcPs
-- TODO: parenthesizeTypeForApp is an overestimate in the case of
-- rendering an infix or record type.
renderField f = wrap $ parenthesizeTypeForApp $ mkLocated $ fieldType f
  where
    wrap = case strictness f of
        NoSrcStrict -> id
        s -> mkLocated . (withEpAnnNotUsed HsBangTy $ noSourceText HsSrcBang NoSrcUnpack s)

renderCon98Decl :: OccNameStr -> HsConDeclDetails' -> ConDecl'
renderCon98Decl name details =
    conDeclH98 (typeRdrName $ unqual name) False [] Nothing details Nothing
  where
#if MIN_VERSION_ghc(9,2,0)
    conDeclH98 = withEpAnnNotUsed ConDeclH98
#elif MIN_VERSION_ghc(8,6,0)
    conDeclH98 n = noExt ConDeclH98 n . builtLoc
#else
    conDeclH98 n _ _ = ConDeclH98 n Nothing
#endif

deriving' :: [HsType'] -> HsDerivingClause'
deriving' = derivingWay Nothing

derivingWay :: Maybe DerivStrategy' -> [HsType'] -> HsDerivingClause'
derivingWay way ts =
#if MIN_VERSION_ghc(9,4,0)
    withEpAnnNotUsed HsDerivingClause (fmap mkLocated way) $ mkLocated $ derivClauseTys $ map sigType ts
#else
    withEpAnnNotUsed HsDerivingClause (fmap builtLoc way) $ mkLocated $ derivClauseTys $ map sigType ts
#endif
  where
#if MIN_VERSION_ghc(9,2,0)
    derivClauseTys [x] = noExt DctSingle x
    derivClauseTys xs = noExt DctMulti xs
#else
    derivClauseTys = id
#endif

derivingStock :: [HsType'] -> HsDerivingClause'
derivingStock = derivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed StockStrategy
#else
    strat = StockStrategy
#endif

derivingNewtype :: [HsType'] -> HsDerivingClause'
derivingNewtype = derivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed NewtypeStrategy
#else
    strat = NewtypeStrategy
#endif

derivingAnyclass :: [HsType'] -> HsDerivingClause'
derivingAnyclass = derivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed AnyclassStrategy
#else
    strat = AnyclassStrategy
#endif

#if MIN_VERSION_ghc(8,6,0)
-- | A `DerivingVia` clause.
--
-- > deriving (Eq, Show) via T
-- > =====
-- > derivingVia (var "T") [var "Eq", var "Show"]
-- Available with @ghc>=8.6@.
derivingVia :: HsType' -> [HsType'] -> HsDerivingClause'
derivingVia t = derivingWay (Just $ strat $ sigType t)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = ViaStrategy . withEpAnnNotUsed XViaStrategyPs
#else
    strat = ViaStrategy
#endif
#endif

standaloneDeriving :: HsType' -> HsDecl'
standaloneDeriving = standaloneDerivingWay Nothing

standaloneDerivingStock :: HsType' -> HsDecl'
standaloneDerivingStock = standaloneDerivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed StockStrategy
#else
    strat = StockStrategy
#endif

standaloneDerivingNewtype :: HsType' -> HsDecl'
standaloneDerivingNewtype = standaloneDerivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed NewtypeStrategy
#else
    strat = NewtypeStrategy
#endif

standaloneDerivingAnyclass :: HsType' -> HsDecl'
standaloneDerivingAnyclass = standaloneDerivingWay (Just strat)
  where
#if MIN_VERSION_ghc(9,2,0)
    strat = withEpAnnNotUsed AnyclassStrategy
#else
    strat = AnyclassStrategy
#endif

standaloneDerivingWay :: Maybe DerivStrategy' -> HsType' -> HsDecl'
standaloneDerivingWay way ty = noExt DerivD derivDecl
  where derivDecl =
#if MIN_VERSION_ghc(9,4,0)
          withEpAnnNotUsed DerivDecl (hsWC $ sigType ty) (fmap mkLocated way) Nothing
#else
          withEpAnnNotUsed DerivDecl (hsWC $ sigType ty) (fmap builtLoc way) Nothing
#endif
        hsWC =
#if MIN_VERSION_ghc(8,6,0)
          noExt HsWC
#else
          id
#endif

-- | Declares multiple pattern signatures of the same type.
--
-- > pattern F, G :: T
-- > =====
-- > patSynSigs ["F", "G"] $ var "T"
patSynSigs :: [OccNameStr] -> HsType' -> HsDecl'
patSynSigs names t =
    sigB $ withEpAnnNotUsed PatSynSig (map (typeRdrName . unqual) names)
        $ sigType t

-- | Declares a pattern signature and its type.
--
-- > pattern F :: T
-- > =====
-- > patSynSigs "F" $ var "T"
patSynSig :: OccNameStr -> HsType' -> HsDecl'
patSynSig n = patSynSigs [n]

-- TODO: patSynBidi, patSynUni

-- | Defines a pattern signature.
--
-- > pattern F a b = G b a
-- > =====
-- > patSynBind "F" ["a", "b"] $ conP "G" [bvar "b", bvar "a"]
patSynBind :: OccNameStr -> [OccNameStr] -> Pat' -> HsDecl'
patSynBind n ns p = bindB $ noExt PatSynBind
                    $ withPlaceHolder (withEpAnnNotUsed PSB (valueRdrName $ unqual n))
                        (prefixCon' (map (valueRdrName . unqual) ns))
                        (builtPat p)
                        ImplicitBidirectional
  where
#if MIN_VERSION_ghc(9,2,0)
    prefixCon' = PrefixCon []
#else
    prefixCon' = PrefixCon
#endif

