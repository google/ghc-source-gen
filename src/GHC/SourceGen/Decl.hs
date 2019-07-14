-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
-- | This module provides combinators for constructing Haskell declarations.
module GHC.SourceGen.Decl
    ( -- * Type declarations
      type'
    , newtype'
    , data'
      -- * Pattern bindings
    , patBind
      -- * Data constructors
    , prefixCon
    , infixCon
    , recordCon
    , Field
    , field
    , strict
    , lazy
      -- * Class declarations
    , class'
    , ClassDecl
    , funDep
      -- * Instance declarations
    , instance'
    , RawInstDecl
    ) where

import BasicTypes (LexicalFixity(Prefix))
import Bag (listToBag)
import HsBinds (HsBindLR(..))
import HsDecls
import HsTypes
    ( ConDeclField(..)
    , FieldOcc(..)
    , HsConDetails(..)
    , HsSrcBang(..)
    , HsType(..)
    , SrcStrictness(..)
    , SrcUnpackedness(..)
    )
import SrcLoc (Located)

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (NoExt(NoExt))
#else
import PlaceHolder (PlaceHolder(..))
#endif

import GHC.SourceGen.Binds
import GHC.SourceGen.Binds.Internal (mkGRHSs)
import GHC.SourceGen.Lit.Internal (noSourceText)
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal

-- | A definition that can appear in the body of a @class@ declaration.
data ClassDecl
    = ClassSig Sig'
    | ClassDefaultMethod HsBind'
    | ClassFunDep [RawRdrName] [RawRdrName]
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
funDep :: [RawRdrName] -> [RawRdrName] -> ClassDecl
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
-- >      ["a"]
-- >      [ typeSig "divMod" $ a --> a --> tuple [a, a]
-- >      , typeSig "div" $ a --> a --> a
-- >      , funBind "div"
-- >          $ matchRhs [var "x", var "y"]
-- >             $ var "fst" @@ (var "divMod" @@ var "x" @@ var "y")
-- >      ]
class'
    :: [HsType'] -- ^ Context
    -> RawRdrName -- ^ Class name
    -> [RawRdrName] -- ^ Type parameters
    -> [ClassDecl] -- ^ Class declarations
    -> HsDecl'
class' context name vars decls
    = noExt TyClD $ ClassDecl
            { tcdCtxt = builtLoc $ map builtLoc context
#if MIN_VERSION_ghc(8,6,0)
            , tcdCExt = NoExt
#else
            , tcdFVs = PlaceHolder
#endif
            , tcdLName = typeRdrName name
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
data RawInstDecl
    = InstSig Sig'
    | InstBind HsBind'

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
-- >       [ matchRhs [var "True"] $ string "True"
-- >       , matchRhs [var "False"] $ string "False"
-- >       ]
-- >   ]
instance' :: HsType' -> [RawInstDecl] -> HsDecl'
instance' ty decls = noExt InstD  $ noExt ClsInstD $ ClsInstDecl
    { cid_poly_ty = sigType ty
#if MIN_VERSION_ghc(8,6,0)
    , cid_ext = NoExt
#endif
    , cid_binds = listToBag [builtLoc b | InstBind b <- decls]
    , cid_sigs = [builtLoc sig | InstSig sig <- decls]
    , cid_tyfam_insts = []
    , cid_datafam_insts = []
    , cid_overlap_mode = Nothing
    }

-- | Declares a type synonym.
--
-- > type A a b = B b a
-- > =====
-- > type' "A" ["a", "b"] $ var "B" @@ var "b" @@ var "a"
type' :: RawRdrName -> [RawRdrName] -> HsType' -> HsDecl'
type' name vars t =
    noExt TyClD $ withPlaceHolder $ noExt SynDecl (typeRdrName name)
        (mkQTyVars vars)
        Prefix
        (builtLoc t)

newOrDataType ::
    NewOrData -> RawRdrName -> [RawRdrName] -> [ConDecl'] -> HsDecl'
newOrDataType newOrData name vars conDecls
    = noExt TyClD $ withPlaceHolder $ withPlaceHolder $
        noExt DataDecl (typeRdrName name)
            (mkQTyVars vars)
            Prefix
            $ noExt HsDataDefn newOrData
                (builtLoc []) Nothing
                Nothing
                (map builtLoc conDecls)
                (builtLoc [])

-- | A newtype declaration.
--
-- > newtype Const a b = Const a
-- > =====
-- > newtype' "Const" ["a", "b"] $ conDecl "Const" [var "a"]
newtype' :: RawRdrName -> [RawRdrName] -> ConDecl' -> HsDecl'
newtype' name vars conD = newOrDataType NewType name vars [conD]

-- | A data declaration.
--
-- > data Either a b = Left a | Right b
-- > =====
-- > data' "Either" ["a", "b"]
-- >   [ conDecl "Left" [var "a"]
-- >   , conDecl "Right" [var "b"]
-- >   ]
data' :: RawRdrName -> [RawRdrName] -> [ConDecl'] -> HsDecl'
data' = newOrDataType DataType

-- | Declares a Haskell-98-style prefix constructor for a data or type
-- declaration.
--
-- > Foo a Int
-- > =====
-- > conDecl "Foo" [field (var "a"), field (var "Int")]
prefixCon :: RawRdrName -> [Field] -> ConDecl'
prefixCon name fields = renderCon98Decl name
    $ PrefixCon $ map renderField fields

-- | Declares a Haskell-98-style infix constructor for a data or type
-- declaration.
--
-- > A b :+: C d
-- > =====
-- > infixCon (field (var "A" @@ var "b")) ":+:" (field (Var "C" @@ var "d"))
infixCon :: Field -> RawRdrName -> Field -> ConDecl'
infixCon f name f' = renderCon98Decl name
    $ InfixCon (renderField f) (renderField f')

-- | Declares Haskell-98-style record constructor for a data or type
-- declaration.
--
-- > A { x :: B, y :: C }
-- > =====
-- > recordCon "A" [("x", var "B"), ("y", var "C")]
recordCon :: RawRdrName -> [(RawRdrName, Field)] -> ConDecl'
recordCon name fields = renderCon98Decl name
    $ RecCon $ builtLoc $ map mkLConDeclField fields
  where
    mkLConDeclField (n, f) =
        builtLoc $ noExt ConDeclField
                        [builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName n]
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

renderCon98Decl :: RawRdrName -> HsConDeclDetails' -> ConDecl'
renderCon98Decl name details = noExt ConDeclH98 (typeRdrName name)
#if MIN_VERSION_ghc(8,6,0)
    (builtLoc False)
    []
#else
    Nothing
#endif
    Nothing
    details
    Nothing

-- | A pattern binding.
--
-- > x = y
-- > =====
-- > patBind (var "x") $ rhs $ var "y"
--
-- > (x, y) = e
-- > =====
-- > patBind (tuple [var "x", var "y"]) $ rhs e
--
-- > (x, y)
-- >   | test = (1, 2)
-- >   | otherwise = (2, 3)
-- > =====
-- > patBind (tuple [var "x", var "y"])
-- >   $ guardedRhs
-- >       [ var "test" `guard` tuple [int 1, int 2]
-- >       , var "otherwise" `guard` [int 2, int 3]
-- >       ]
patBind :: Pat' -> RawGRHSs -> HsDecl'
patBind p g =
    bindB
        $ withPlaceHolder
            (withPlaceHolder
                (noExt PatBind (builtPat p) (mkGRHSs g)))
        $ ([],[])
