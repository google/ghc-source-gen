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
    , conP_
    , recordConP
    , strictP
    , lazyP
    , sigP
    ) where

import GHC.Hs.Type
import GHC.Hs.Pat hiding (LHsRecField')

import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Pat.Internal
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal (patSigType)

#if MIN_VERSION_ghc(9,10,0)
import GHC.Parser.Annotation (EpAnn(..), EpToken(..), noAnn, noSpanAnchor)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation (EpAnn(..))
#endif
#if MIN_VERSION_ghc(9,6,0) && !MIN_VERSION_ghc(9,10,0)
import GHC (noHsTok)
#endif

-- | A wild pattern (@_@).
wildP :: Pat'
wildP = noExtOrPlaceHolder WildPat

-- | An as-pattern.
--
-- > a@B
-- > =====
-- > asP "a" (var "B")
asP :: RdrNameStr -> Pat' -> Pat'
v `asP` p =
#if MIN_VERSION_ghc(9,10,0)
  AsPat (EpTok noSpanAnchor) (valueRdrName v)
    (builtPat $ parenthesize p)
#elif MIN_VERSION_ghc(9,6,0)
  withEpAnnNotUsed AsPat (valueRdrName v)
    noHsTok
    (builtPat $ parenthesize p)
#else
  withEpAnnNotUsed AsPat (valueRdrName v)
    (builtPat $ parenthesize p)
#endif

-- | A pattern constructor.
--
-- > A b c
-- > =====
-- > conP "A" [bvar "b", bvar "c"]
conP :: RdrNameStr -> [Pat'] -> Pat'
conP c pats = conPat (valueRdrName c) (prefixCon (map (builtPat . parenthesize) pats))
  where
#if MIN_VERSION_ghc(9,10,0)
    conPat = ConPat noAnn
#elif MIN_VERSION_ghc(9,0,0)
    conPat = withEpAnnNotUsed ConPat
#else
    conPat = ConPatIn
#endif
#if !MIN_VERSION_ghc(9,13,0) && MIN_VERSION_ghc(9,2,0)
    prefixCon = PrefixCon []
#else
    prefixCon = PrefixCon
#endif

-- | A pattern constructor with no arguments.
--
-- > A
-- > =====
-- > conP_ "A"
conP_ :: RdrNameStr -> Pat'
conP_ c = conP c []

recordConP :: RdrNameStr -> [(RdrNameStr, Pat')] -> Pat'
recordConP c fs =
#if MIN_VERSION_ghc(9,10,0)
  ConPat noAnn
#elif MIN_VERSION_ghc(9,0,0)
  withEpAnnNotUsed ConPat
#else
  ConPatIn
#endif
  (valueRdrName c)
        $ RecCon $ noExt HsRecFields (map mkRecField fs) Nothing -- No ".."
  where
    mkRecField :: (RdrNameStr, Pat') -> LHsRecField' LPat'
    mkRecField (f, p) =
#if MIN_VERSION_ghc(9,10,0)
        mkLocated $ HsFieldBind
            { hfbAnn = noAnn
            , hfbLHS = mkLocated $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hfbRHS = builtPat p
            , hfbPun = False      
#elif MIN_VERSION_ghc(9,4,0)
        mkLocated $ HsFieldBind
            { hfbAnn = EpAnnNotUsed
            , hfbLHS = mkLocated $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hfbRHS = builtPat p
            , hfbPun = False
#else
        mkLocated $ HsRecField
            { hsRecFieldLbl =
                builtLoc $ withPlaceHolder $ noExt FieldOcc $ valueRdrName f
            , hsRecFieldArg = builtPat p
            , hsRecPun = False
#if MIN_VERSION_ghc(9,2,0)
            , hsRecFieldAnn = EpAnnNotUsed
#endif
#endif
            }

-- | A bang-pattern.
--
-- > !x
-- > =====
-- > strictP (bvar x)
strictP :: Pat' -> Pat'
#if MIN_VERSION_ghc(9,10,0)
strictP = BangPat noAnn . builtPat . parenthesize
#else
strictP = withEpAnnNotUsed BangPat . builtPat . parenthesize
#endif

-- | A lazy pattern match.
--
-- > ~(A x)
-- > =====
-- > lazyP (conP "A" [bvar x])
lazyP :: Pat' -> Pat'
#if MIN_VERSION_ghc(9,10,0)
lazyP = LazyPat noAnn . builtPat . parenthesize
#else
lazyP = withEpAnnNotUsed LazyPat . builtPat . parenthesize
#endif

-- | A pattern type signature
--
-- > x :: y
-- > =====
-- > sigPat (bvar "x") (var "y")
sigP :: Pat' -> HsType' -> Pat'
#if MIN_VERSION_ghc(9,10,0)
sigP p t = SigPat noAnn (builtPat p) (patSigType t)
#elif MIN_VERSION_ghc(8,8,0)
sigP p t = withEpAnnNotUsed SigPat (builtPat p) (patSigType t)
#elif MIN_VERSION_ghc(8,6,0)
sigP p t = SigPat (patSigType t) (builtPat p)
#else
sigP p t = SigPatIn (builtPat p) (patSigType t)
#endif
