-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
-- | This module provides combinators for constructing Haskell patterns.
module GHC.SourceGen.Pat
    ( Pat'
    , wildP
    , asP
    , conP
    , recordConP
    , strictP
    , lazyP
    ) where

import SrcLoc (unLoc)
import HsTypes
import HsPat hiding (LHsRecField')

import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Overloaded (par)
import GHC.SourceGen.Expr.Internal (litNeedsParen, overLitNeedsParen)
import GHC.SourceGen.Syntax.Internal

-- | A wild pattern (@_@).
wildP :: Pat'
wildP = noExtOrPlaceHolder WildPat

-- | An as-pattern.
--
-- > a@B
-- > =====
-- > asP "a" (var "B")
asP :: RdrNameStr -> Pat' -> Pat'
v `asP` p = noExt AsPat (valueRdrName v) $ builtPat $ parenthesize p

-- | A pattern constructor.
--
-- > A b c
-- > =====
-- > conP "A" [var "b", var "c"]
conP :: RdrNameStr -> [Pat'] -> Pat'
conP c xs = ConPatIn (valueRdrName c) $ PrefixCon
                $ map (builtPat . parenthesize) xs

-- Note: GHC>=8.6 inserts parentheses automatically when pretty-printing patterns.
-- When we stop supporting lower versions, we may be able to simplify this.
parenthesize :: Pat' -> Pat'
parenthesize p
    | needsPar p = par p
    | otherwise = p

needsPar :: Pat' -> Bool
#if MIN_VERSION_ghc(8,6,0)
needsPar (LitPat _ l) = litNeedsParen l
needsPar (NPat _ l _ _) = overLitNeedsParen $ unLoc l
#else
needsPar (LitPat l) = litNeedsParen l
needsPar (NPat l _ _ _) = overLitNeedsParen $ unLoc l
#endif
needsPar (ConPatIn _ (PrefixCon xs)) = not $ null xs
needsPar (ConPatIn _ (InfixCon _ _)) = True
needsPar ConPatOut{} = True
#if MIN_VERSION_ghc(8,6,0)
needsPar SigPat{} = True
#else
needsPar SigPatIn{} = True
needsPar SigPatOut{} = True
#endif
needsPar _ = False

recordConP :: RdrNameStr -> [(RdrNameStr, Pat')] -> Pat'
recordConP c fs
    = ConPatIn (valueRdrName c)
        $ RecCon $ HsRecFields (map mkRecField fs) Nothing -- No ".."
  where
    mkRecField :: (RdrNameStr, Pat') -> LHsRecField' LPat'
    mkRecField (f, p) =
        builtLoc $ HsRecField
            { hsRecFieldLbl =
                builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hsRecFieldArg = builtPat p
            , hsRecPun = False
            }

-- | A bang-pattern.
--
-- > !x
-- > =====
-- > strictP (var x)
strictP :: Pat' -> Pat'
strictP = noExt BangPat . builtPat . parenthesize

-- | A lazy pattern match.
--
-- > ~(A x)
-- > =====
-- > lazyP (conP "A" [var x])
lazyP :: Pat' -> Pat'
lazyP = noExt LazyPat . builtPat . parenthesize
