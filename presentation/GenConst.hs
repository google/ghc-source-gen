{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHC.SourceGen
import GHC (runGhc)
import GHC.Paths (libdir)

constModule :: HsModule'
constModule =
    module' (Just "Const") (Just [var "const"]) []
        [ typeSig "const" $ var "a" --> var "b" --> var "a"
        , funBind "const" $ match [bvar "x", wildP] (var "x")
        ]

main = runGhc (Just libdir) $ putPpr constModule

