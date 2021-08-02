-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
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

#if MIN_VERSION_ghc(9,0,1)
import GHC.Types.Basic (LexicalFixity(Prefix))
import GHC.Data.Bag (listToBag)
import GHC.Types.SrcLoc (Located, LayoutInfo(NoLayoutInfo))
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
#else
import BasicTypes (LexicalFixity(Prefix))
import Bag (listToBag)
import SrcLoc (Located)
#endif

#if !MIN_VERSION_ghc(8,6,0)
import BasicTypes (DerivStrategy(..))
#endif
import GHC.Hs.Binds
import GHC.Hs.Decls
#if MIN_VERSION_ghc(9,0,1)
import GHC.Hs.Type
#else
import GHC.Hs.Types
#endif
    ( ConDeclField(..)
    , FieldOcc(..)
    , HsConDetails(..)
    , HsImplicitBndrs (..)
    , HsSrcBang(..)
    , HsType(..)
#if MIN_VERSION_ghc(8,6,0)
    , HsWildCardBndrs (..)
#endif
#if MIN_VERSION_ghc(8,8,0)
    , HsArg(..)
#endif
    , SrcStrictness(..)
    , SrcUnpackedness(..)
#if MIN_VERSION_ghc(9,0,1)
    , HsScaled (HsScaled)
    , HsArrow (HsUnrestrictedArrow)
#endif
    )

#if MIN_VERSION_ghc(8,10,0)
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
    -> [HsTyVarBndrUnit'] -- ^ Type parameters
    -> [ClassDecl] -- ^ Class declarations
    -> HsDecl'
class' context name vars decls
    = noExt TyClD $ ClassDecl
            { tcdCtxt = builtLoc $ map builtLoc context
#if MIN_VERSION_ghc(8,10,0)
            , tcdCExt = NoLayoutInfo 
#elif MIN_VERSION_ghc(8,6,0)
            , tcdCExt = NoExt
#else
            , tcdFVs = PlaceHolder
#endif
            , tcdLName = typeRdrName $ unqual name
            , tcdTyVars = mkQTyVars vars
            , tcdFixity = Prefix
            , tcdFDs = [ builtLoc (map typeRdrName xs, map typeRdrName ys)
                       | ClassFunDep xs ys <- decls
                       ]
            , tcdSigs = [builtLoc sig | ClassSig sig <- decls]
            , tcdMeths =
                listToBag [builtLoc bind | ClassDefaultMethod bind <- decls]
            , tcdATs = []  -- Associated types
            , tcdATDefs = []  -- Associated type defaults
            , tcdDocs = []  -- Haddocks
            }

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
#if MIN_VERSION_ghc(8,10,0)
    , cid_ext = NoExtField
#elif MIN_VERSION_ghc(8,6,0)
    , cid_ext = NoExt
#endif
    , cid_binds = listToBag [builtLoc b | InstBind b <- decls]
    , cid_sigs = [builtLoc sig | InstSig sig <- decls]
    , cid_tyfam_insts = [builtLoc $ t | InstTyFam t <- decls]
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
        $ TyFamInstDecl
        $ implicitBndrs
        $ noExt FamEqn (typeRdrName name)
#if MIN_VERSION_ghc(8,8,0)
            Nothing -- eqn binders
            (map (HsValArg . builtLoc) params)
#else
            (map builtLoc params)
#endif
            Prefix
            (builtLoc ty)

-- | Declares a type synonym.
--
-- > type A a b = B b a
-- > =====
-- > type' "A" [bvar "a", bvar "b"] $ var "B" @@ var "b" @@ var "a"
type' :: OccNameStr -> [HsTyVarBndrUnit'] -> HsType' -> HsDecl'
type' name vars t =
    noExt TyClD $ withPlaceHolder $ noExt SynDecl (typeRdrName $ unqual name)
        (mkQTyVars vars)
        Prefix
        (builtLoc t)

newOrDataType
    :: NewOrData
    -> OccNameStr
    -> [HsTyVarBndrUnit']
    -> [ConDecl']
    -> [HsDerivingClause']
    -> HsDecl'
newOrDataType newOrData name vars conDecls derivs
    = noExt TyClD $ withPlaceHolder $ withPlaceHolder $
        noExt DataDecl (typeRdrName $ unqual name)
            (mkQTyVars vars)
            Prefix
            $ noExt HsDataDefn newOrData
                (builtLoc []) Nothing
                Nothing
                (map builtLoc conDecls)
                (builtLoc $ map builtLoc derivs)

-- | A newtype declaration.
--
-- > newtype Const a b = Const a deriving Eq
-- > =====
-- > newtype' "Const" [bvar "a", bvar "b"]
-- >    (conDecl "Const" [var "a"])
-- >    [var "Show"]
newtype' :: OccNameStr -> [HsTyVarBndrUnit'] -> ConDecl' -> [HsDerivingClause'] -> HsDecl'
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
data' :: OccNameStr -> [HsTyVarBndrUnit'] -> [ConDecl'] -> [HsDerivingClause'] -> HsDecl'
data' = newOrDataType DataType

-- | Declares a Haskell-98-style prefix constructor for a data or type
-- declaration.
--
-- > Foo a Int
-- > =====
-- > conDecl "Foo" [field (var "a"), field (var "Int")]
prefixCon :: OccNameStr -> [Field] -> ConDecl'
prefixCon name fields =
    renderCon98Decl
        name
            $ PrefixCon
#if MIN_VERSION_ghc(9,0,1)
                $ map (HsScaled (HsUnrestrictedArrow NormalSyntax) . renderField)
#else
                $ map renderField
#endif
                     fields

-- | Declares a Haskell-98-style infix constructor for a data or type
-- declaration.
--
-- > A b :+: C d
-- > =====
-- > infixCon (field (var "A" @@ var "b")) ":+:" (field (Var "C" @@ var "d"))
infixCon :: Field -> OccNameStr -> Field -> ConDecl'
infixCon f name f' = renderCon98Decl name
    $ InfixCon
        (
#if MIN_VERSION_ghc(9,0,1)
            HsScaled (HsUnrestrictedArrow NormalSyntax) $
#endif
                renderField f
        )
        (
#if MIN_VERSION_ghc(9,0,1)
            HsScaled (HsUnrestrictedArrow NormalSyntax) $ 
#endif
                renderField f'
        )

-- | Declares Haskell-98-style record constructor for a data or type
-- declaration.
--
-- > A { x :: B, y :: C }
-- > =====
-- > recordCon "A" [("x", var "B"), ("y", var "C")]
recordCon :: OccNameStr -> [(OccNameStr, Field)] -> ConDecl'
recordCon name fields = renderCon98Decl name
    $ RecCon $ builtLoc $ map mkLConDeclField fields
  where
    mkLConDeclField (n, f) =
        builtLoc $ noExt ConDeclField
                        [builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName $ unqual n]
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

renderField :: Field -> Located HsType'
-- TODO: parenthesizeTypeForApp is an overestimate in the case of
-- rendering an infix or record type.
renderField f = wrap $ parenthesizeTypeForApp $ builtLoc $ fieldType f
  where
    wrap = case strictness f of
        NoSrcStrict -> id
        s -> builtLoc . (noExt HsBangTy $ noSourceText HsSrcBang NoSrcUnpack s)

renderCon98Decl :: OccNameStr -> HsConDeclDetails' -> ConDecl'
renderCon98Decl name details = noExt ConDeclH98 (typeRdrName $ unqual name)
#if MIN_VERSION_ghc(8,6,0)
    (builtLoc False)
    []
#else
    Nothing
#endif
    Nothing
    details
    Nothing

deriving' :: [HsType'] -> HsDerivingClause'
deriving' = derivingWay Nothing

derivingWay :: Maybe DerivStrategy' -> [HsType'] -> HsDerivingClause'
derivingWay way ts =
    noExt HsDerivingClause (fmap builtLoc way) $ builtLoc $ map sigType ts

derivingStock :: [HsType'] -> HsDerivingClause'
derivingStock = derivingWay (Just StockStrategy)

derivingNewtype :: [HsType'] -> HsDerivingClause'
derivingNewtype = derivingWay (Just NewtypeStrategy)

derivingAnyclass :: [HsType'] -> HsDerivingClause'
derivingAnyclass = derivingWay (Just AnyclassStrategy)

#if MIN_VERSION_ghc(8,6,0)
-- | A `DerivingVia` clause.
--
-- > deriving (Eq, Show) via T
-- > =====
-- > derivingVia (var "T") [var "Eq", var "Show"]
-- Available with @ghc>=8.6@.
derivingVia :: HsType' -> [HsType'] -> HsDerivingClause'
derivingVia t = derivingWay (Just $ ViaStrategy $ sigType t)
#endif

standaloneDeriving :: HsType' -> HsDecl'
standaloneDeriving = standaloneDerivingWay Nothing

standaloneDerivingStock :: HsType' -> HsDecl'
standaloneDerivingStock = standaloneDerivingWay (Just StockStrategy)

standaloneDerivingNewtype :: HsType' -> HsDecl'
standaloneDerivingNewtype = standaloneDerivingWay (Just NewtypeStrategy)

standaloneDerivingAnyclass :: HsType' -> HsDecl'
standaloneDerivingAnyclass = standaloneDerivingWay (Just AnyclassStrategy)

standaloneDerivingWay :: Maybe DerivStrategy' -> HsType' -> HsDecl'
standaloneDerivingWay way ty = noExt DerivD derivDecl
  where derivDecl =
          noExt DerivDecl (hsWC hsIB) (fmap builtLoc way) Nothing
        hsIB =
          withPlaceHolder $ noExtOrPlaceHolder HsIB (builtLoc ty)
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
    sigB $ noExt PatSynSig (map (typeRdrName . unqual) names)
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
                    $ withPlaceHolder (noExt PSB (valueRdrName $ unqual n))
                        (PrefixCon (map (valueRdrName . unqual) ns))
                        (builtPat p)
                        ImplicitBidirectional

