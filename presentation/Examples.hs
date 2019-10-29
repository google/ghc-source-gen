{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Examples where

import GHC.SourceGen
import GHC (runGhc, noLoc, NoExt(..))
import GHC.Paths (libdir)
import HsExpr

renderExpr :: HsExpr' -> IO ()
renderExpr e = runGhc (Just libdir) $ putPpr e

renderDecl :: HsDecl' -> IO ()
renderDecl d = runGhc (Just libdir) $ putPpr d



test1 = var "f" @@ var "g" @@ var "h"

test2 = (var "f" @@ var "g") @@ var "h"

test3 = var "f" @@ (var "g" @@ var "h")

test4 = var "f" @@ tuple [var "g", var "h"]

{-
id x = x
-}

id1 = funBind "id" $ match [bvar "x"] $ var "x"

{-
not True = False
not False = True
-}

not1 =
    funBinds "not"
        [ match [conP_ "True"] $ var "False"
        , match [conP_ "False"] $ var "True"
        ]

{-
not x
    | x = False
    | otherwise = True
-}

not2 =
    funBinds "not"
        [ matchGRHSs [bvar "x"]
            $ guardedRhs
                [ guard (var "x") (var "False")
                , guard (var "otherwise") (var "True")
                ]
        ]


{-
not x
    | x = False
    | otherwise = y
  where
    y = True
-}

not3 =
    funBinds "not"
        [ matchGRHSs [conP_ "x"]
            $ guardedRhs
                [ guard (var "x") (var "False")
                , guard (var "otherwise") (var "y")
                ]
                `where'` [valBind "y" (var "True")]
        ]
