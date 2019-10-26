{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module GenParens where

import GHC.SourceGen
import GHC (runGhc, noLoc, NoExt(..))
import GHC.Paths (libdir)
import HsExpr

render :: HsExpr' -> IO ()
render e = runGhc (Just libdir) $ putPpr e

myApp :: HsExpr' -> HsExpr' -> HsExpr'
myApp x y = HsApp NoExt (noLoc x) (noLoc y)




test1 = var "f" @@ var "g" @@ var "h"

test2 = (var "f" @@ var "g") @@ var "h"

test3 = var "f" @@ (var "g" @@ var "h")

simpleApp :: HsExpr' -> HsExpr' -> HsExpr'
simpleApp x y = HsApp NoExt (noLoc x) (noLoc y)


test4 = var "f" `simpleApp` (var "g" @@ var "h")
