{-# LANGUAGE CPP #-}
-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides utilities for rendering GHC syntax as strings.
module GHC.SourceGen.Pretty
    ( showPpr
    , putPpr
    , hPutPpr
    ) where

import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Utils.Outputable
import System.IO

#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Ppr (printForUser, showPpr)
#endif

hPutPpr :: Outputable a => Handle -> a -> Ghc ()
hPutPpr h x = do
    dflags <- getDynFlags
    liftIO $ printForUser dflags h neverQualify
        AllTheWay $ ppr x

putPpr :: Outputable a => a -> Ghc ()
putPpr = hPutPpr stdout
