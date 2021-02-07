{-# LANGUAGE CPP #-}
module GHC.Hs.Type
#if MIN_VERSION_ghc(8,10,0)
(module GHC.Hs.Types) where
import GHC.Hs.Types
#else
(module HsTypes) where
import HsTypes
#endif
