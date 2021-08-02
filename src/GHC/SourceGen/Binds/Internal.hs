-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Binds.Internal where


import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Expr (MatchGroup(..), Match(..), GRHSs(..))
#if MIN_VERSION_ghc(9,0,1)
import GHC.Types.Basic (Origin(Generated))
import GHC.Data.Bag (listToBag)
import GHC.Types.SrcLoc (Located)
#else
import BasicTypes (Origin(Generated))
import Bag (listToBag)
import SrcLoc (Located)
#endif

#if !MIN_VERSION_ghc(8,6,0)
import PlaceHolder (PlaceHolder(..))
#endif

import GHC.SourceGen.Pat.Internal (parenthesize)
import GHC.SourceGen.Syntax.Internal

-- | A binding definition inside of a @let@ or @where@ clause.
--
-- 'RawValBind' definitions may be constructed using its instance of
-- 'HasValBind'.  For more details, see the documentation of that function, and
-- of "GHC.SourceGen.Binds" overall.
data RawValBind
    = SigV Sig'
    | BindV HsBind'

valBinds :: [RawValBind] -> HsLocalBinds'
-- This case prevents GHC from printing an empty "where" clause:
valBinds [] = noExt EmptyLocalBinds
valBinds vbs =
    noExt HsValBinds
#if MIN_VERSION_ghc(8,6,0)
        $ noExt ValBinds
#else
        $ noExt ValBindsIn
#endif
            (listToBag $ map builtLoc binds)
            (map builtLoc sigs)
  where
    sigs = [s | SigV s <- vbs]
    binds = [b | BindV b <- vbs]

-- | A single function pattern match, including an optional "where" clause.
--
-- For example:
--
-- > f x
-- >    | cond = y
-- >    | otherwise = z
-- >  where
-- >    y = ...
-- >    z = ...
data RawMatch = RawMatch
    { rawMatchPats :: [Pat']
    , rawMatchGRHSs :: RawGRHSs
    }

-- | A set of match guards plus an optional "where" clause.
--
-- This type is used in matches and in multi-way if expressions.
--
-- For example:
--
-- >    | cond = y
-- >    | otherwise = z
-- >  where
-- >    y = ...
-- >    z = ...
data RawGRHSs = RawGRHSs
    { rawGRHSs :: [GuardedExpr]
    , rawGRHSWhere :: [RawValBind]
    }

matchGroup :: HsMatchContext' -> [RawMatch] -> MatchGroup' (Located HsExpr')
matchGroup context matches =
    noExt MG (builtLoc $ map (builtLoc . mkMatch) matches)
#if !MIN_VERSION_ghc(8,6,0)
                            [] PlaceHolder
#endif
                            Generated
  where
    mkMatch :: RawMatch -> Match' (Located HsExpr')
    mkMatch r = noExt Match context
                    (map builtPat $ map parenthesize $ rawMatchPats r)
                    (mkGRHSs $ rawMatchGRHSs r)

mkGRHSs :: RawGRHSs -> GRHSs' (Located HsExpr')
mkGRHSs g = noExt GRHSs
                (map builtLoc $ rawGRHSs g)
                (builtLoc $ valBinds $ rawGRHSWhere g)

-- | An expression with a single guard.
--
-- For example:
--
-- > | otherwise = ()
type GuardedExpr = GRHS' (Located HsExpr')

-- | Syntax types which can declare/define functions.  For example:
-- declarations, or the body of a class declaration or class instance.
--
-- To declare the type of a function or value, use
-- 'GHC.SourceGen.Binds.typeSig' or 'GHC.SourceGen.Binds.typeSigs'.
--
-- To define a function, use 
-- 'GHC.SourceGen.Binds.funBind' or 'GHC.SourceGen.Binds.funBinds'.
--
-- To define a value, use
-- 'GHC.SourceGen.Binds.valBind' or 'GHC.SourceGen.Binds.valBindGuarded'.
class HasValBind t where
    sigB :: Sig' -> t
    bindB :: HsBind' -> t

instance HasValBind HsDecl' where
    sigB = noExt SigD
    bindB = noExt ValD

instance HasValBind RawValBind where
    sigB = SigV
    bindB = BindV
