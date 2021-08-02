-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}

-- | This module provides utilities for rendering GHC syntax as strings.
module GHC.SourceGen.Pretty
    ( showPpr
    , putPpr
    , hPutPpr
    ) where

#if MIN_VERSION_ghc(9,0,1)
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Utils.Outputable
#else
import DynFlags
import GhcMonad
import Outputable
#endif
import System.IO

hPutPpr :: Outputable a => Handle -> a -> Ghc ()
hPutPpr h x = do
    dflags <- getDynFlags
    liftIO $
        printForUser
            dflags
            h
            neverQualify
#if MIN_VERSION_ghc(9,0,1)
                AllTheWay
#endif
                $ ppr x

putPpr :: Outputable a => a -> Ghc ()
putPpr = hPutPpr stdout
