-- A module for changing behavior based on the version of GHC.
{-# LANGUAGE CPP #-}
module GhcVersion where

import Data.Version
import Text.ParserCombinators.ReadP

ghcVersion :: Version
ghcVersion = case readP_to_S (parseVersion <* eof) VERSION_ghc of
    [(v,"")] -> v
    _ -> error $ "Unable to parse GHC version " ++ show VERSION_ghc

ifGhc88 :: a -> a -> a
ifGhc88 x y = if makeVersion [8,8] <= ghcVersion
                then x
                else y
