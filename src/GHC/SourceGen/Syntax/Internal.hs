-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.SourceGen.Syntax.Internal where

import SrcLoc (SrcSpan, Located, GenLocated(..), mkGeneralSrcSpan)

#if MIN_VERSION_ghc(8,8,0)
import BasicTypes (PromotionFlag(..))
#else
import HsTypes (Promoted(..))
#endif

#if MIN_VERSION_ghc(8,6,0)
import HsExtension (NoExt(NoExt))
#else
import PlaceHolder(PlaceHolder(..))
#endif
import GHC.SourceGen.Syntax

#if MIN_VERSION_ghc(8,6,0)
noExt :: (NoExt -> a) -> a
noExt = ($ NoExt)

noExtOrPlaceHolder :: (NoExt -> a) -> a
noExtOrPlaceHolder = noExt

withPlaceHolder :: a -> a
withPlaceHolder = id

#else

noExt :: a -> a
noExt = id

noExtOrPlaceHolder :: (PlaceHolder -> a) -> a
noExtOrPlaceHolder = withPlaceHolder

withPlaceHolder :: (PlaceHolder -> a) -> a
withPlaceHolder = ($ PlaceHolder)

#endif

builtSpan :: SrcSpan
builtSpan = mkGeneralSrcSpan "<ghc-source-gen>"

builtLoc :: e -> Located e
builtLoc = L builtSpan

-- In GHC-8.8, source locations for Pat aren't stored in each node, and
-- LPat is a synonym for Pat.
#if MIN_VERSION_ghc(8,8,0)
builtPat :: Pat' -> Pat'
builtPat = id
#else
builtPat :: Pat' -> Located Pat'
builtPat = builtLoc
#endif

#if MIN_VERSION_ghc(8,8,0)
promoted, notPromoted :: PromotionFlag
promoted = IsPromoted
notPromoted = NotPromoted
#else
promoted, notPromoted :: Promoted
promoted = Promoted
notPromoted = NotPromoted
#endif
