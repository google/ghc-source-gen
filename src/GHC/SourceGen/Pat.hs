-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

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

import HsTypes
import HsPat hiding (LHsRecField')

import GHC.SourceGen.Name.Internal
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
v `asP` p = noExt AsPat (valueRdrName v) $ builtPat p

-- | A pattern constructor.
--
-- > A b c
-- > =====
-- > conP "A" [var "b", var "c"]
conP :: RdrNameStr -> [Pat'] -> Pat'
conP c xs = ConPatIn (valueRdrName c) $ PrefixCon $ map builtPat xs

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
strictP = noExt BangPat . builtPat

-- | A lazy pattern match.
--
-- > ~(A x)
-- > =====
-- > lazyP (conP "A" [var x])
lazyP :: Pat' -> Pat'
lazyP = noExt LazyPat . builtPat
