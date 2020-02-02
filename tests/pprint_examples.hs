-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | Simple example using this package
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import GHC.Paths (libdir)
import GHC (runGhc)
import Outputable (Outputable)

import GHC.SourceGen

main :: IO ()
main = mapM_ run [test1, test2, test3, test4, test5]
  where
    run act = putStrLn "========" >> act

pprint :: Outputable a => a -> IO ()
pprint x = runGhc (Just libdir) $ putPpr x

test1 :: IO ()
test1 = pprint $ tuple
    [ var "Foo.abc"
    , overLabel "def"
    , char 'g'
    , let' [ typeSig "result" $ var "A" @@ var "B"
           , funBind "result"
                $ match [bvar "x", wildP]
                    $ var "foo" @@ char 'c'
           ]
        (var "result")
    ]

test2 :: IO ()
test2 = pprint $ module' (Just "Foo") (Just [var "efg"]) []
    [ typeSigs ["efg", "h"] $ tuple [var "A", var "B"]
    , funBind "efg"
        $ matchGRHSs []
        $ rhs (char 'a')
            `where'` [ typeSig "q" $ var "Q"
                     , funBind "q" $ matchGRHSs []
                        $ guardedRhs [var "True" `guard` char 'q']
                     ]
    , funBind "f"
        $ matchGRHSs [bvar "x", bvar "y"]
        $ rhs
            (case' (var "y")
                        [match [wildP] $ var "x"])
            `where'` [funBind "q" $ match [] $ char 't']
    ]

test3 :: IO ()
test3 = pprint $ module' Nothing Nothing []
    [ funBind "lambdas" $ match [] $ lambda [bvar "y"]
                    $ lambdaCase [match [bvar "z"] (char 'a')]
    , funBinds "ifs"
        [ match [bvar "x"] $ if' (var "b") (var "t") (var "f")
        , match [bvar "y"] $ multiIf [guard (var "False") $ char 'f'
                                       , guard (var "True") $ char 't'
                                       ]
        , match [bvar "z"] $ multiIf
            [ guard (var "f" @@ var "x") $ string "f"
            , guard (var "g" @@ var "x") $ string "g"
            , guard (var "otherwise") $ string "h"
            ]
        ]
    , funBind "do'"
        $ match [] (do' [ bvar "x" <-- var "act"
                        , stmt $ var "return" @@ var "x"
                        ])
    , typeSig "types"
        $ forall' [bvar "x", bvar "y"]
        $ [var "Show" @@ var "x"] ==> var "y"
    , typeSig "types'"
        $ [var "Show" @@ var "x"] ==>
            (forall' [bvar "x", bvar "y"]
                $ var "y")
    , funBind "swap"
        $ match [tuple [bvar "x", bvar "y"]]
            $ tuple [var "y", var "x"]
    , funBind "char" $ match [char 'a'] (char 'b')
    , funBind "string" $ match [string "abc"] (string "def")
    , funBind "as"
        $ match [asP "x" (tuple [bvar "y", bvar "z"])]
            (var "x")
    , funBind "con"
        $ match [conP "A" [bvar "b", conP "C" [bvar "d"]]]
            $ tuple [var "b", var "d"]
    , funBind "ops"
        $ match [bvar "x", bvar "y"]
            $ op (var "x") "+" (var "y")
    , funBinds "ops'"
        [ match [] (op (int 1) "*"
                        (op (int 2) "+" (int 3)))
        , match [] (op (var "A" @@ var "x") "*"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z")))
        , match [] (op (var "A" @@ var "x") "mult"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z")))
        ]
    , funBinds "cons'"
        [ match [] (var "X" @@ int 1 @@
                        (var "Y" @@ int 2 @@ int 3)
                        @@ var "Z")
        , match [] (var "f" @@ par (var "g" @@ var "x"))
        ]
    , typeSig "f" $ var "X" @@ var "a" @@
                        (var "Y" @@ var "b" @@ var "c")
                        @@ var "Z"
    , typeSig "g" $ op (var "A" @@ var "x") "*"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z"))
    , class' [var "A" @@ var "a"] "B" [bvar "b", bvar "b'"]
        [ typeSig "f" $ var "b" --> var "b'"
        , funBind "f" $ match [] $ var "id"
        ]
    , class' [] "F" [bvar "a", bvar "b", bvar "c"]
        [ funDep ["a", "b"] ["c"]
        , funDep ["a"] ["b", "c"]
        ]
    , class' [] "Ident" [bvar "a", bvar "b"]
        [ funDep ["a"] ["b"]
        , funDep ["b"] ["a"]
        , typeSig "ident" $ var "a" --> var "b"
        ]
    , type' "A" [bvar "b", bvar "c"] $ var "D"
    , data' "A" [bvar "b", bvar "c"]
        [ prefixCon "A" [field (var "b"), field (var "c")]
        , prefixCon "D" []
        ]
        [deriving' [var "X", var "Y"]]
    , newtype' "A" [bvar "b", bvar "c"] (prefixCon "A" [field (var "b")])
        [deriving' [var "X", var "Y"]]
    , instance' (var "A" @@ var "b" @@ var "c")
        [ typeSig "f" $ var "b" --> var "c"
        , funBind "f" $ match [] $ var "undefined"
        ]
    , let a = var "a"
      in class'
           [var "Real" @@ a, var "Enum" @@ a]
           "Integral"
           [bvar "a"]
           [ typeSig "divMod" $ a --> a --> tuple [a, a]
           , typeSig "div" $ a --> a --> a
           , funBind "div"
               $ match [bvar "x", bvar "y"]
                  $ var "fst" @@ (var "divMod" @@ var "x" @@ var "y")
           ]
    , instance' (var "Show" @@ var "Bool")
        [ typeSig "show" $ var "Bool" --> var "String"
        , funBinds "show"
            [ match [conP "True" []] $ string "True"
            , match [conP "False" []] $ string "False"
            ]
        ]
    , data' "X" [bvar "b"]
        [ prefixCon "X"
            [ field $ var "A" @@ var "b"
            , strict $ field $ var "A" @@ var "b"
            , lazy $ field $ var "A" @@ var "b"
            ]
        , prefixCon ":+"
            [ field $ var "A" @@ var "b"
            , strict $ field $ var "A" @@ var "b"
            , lazy $ field $ var "A" @@ var "b"
            ]
        , infixCon
            (strict $ field $ var "A" @@ var "b")
            "Y"
            (lazy $ field $ var "A" @@ var "b")
        , infixCon
            (strict $ field $ var "A" @@ var "b")
            ":*"
            (lazy $ field $ var "A" @@ var "b")
        , infixCon
            (field $ var "A" @@ var "b")
            ":*"
            (field $ var "A" @@ var "b")
        , recordCon "Z"
            [ ("x", field $ var "Int")
            , ("y", field $ var "A" @@ var "b")
            , ("y", strict $ field $ var "A" @@ var "b")
            , ("y", lazy $ field $ var "A" @@ var "b")
            ]
        ]
        []
    , funBind "strictness"
        $ match
            [strictP (conP "A" [bvar "b"]),
             lazyP (conP "A" [bvar "b"])
            ] (char 'x')
    , typeSig "unit" $ unit --> unit
    , funBind "unit" $ match [unit] unit
    ]

test4 :: IO ()
test4 = pprint constModule

test5 :: IO ()
test5 = pprint $ module' (Just "M") (Just exports) imports []
  where
    exports = [ var "a"
              , var "A"
              , thingAll "B"
              , thingWith "C" ["d", "E"]
              ]
    imports = [ qualified' $ import' "A"
              , import' "B" `as'` "C"
              , import' "D" `exposing` [var "d"]
              , import' "E" `hiding` [var "e"]
              ]

constModule :: HsModule'
constModule = module' (Just "Const") (Just [var "const"]) []
    [ typeSig "const" $ a --> b --> a
    , funBind "const" $ match [wildP, x] x
    ]
  where
    a = bvar "a"
    b = bvar "b"
    x = bvar "x"
