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
                $ matchRhs [var "x", wildP]
                    $ var "foo" @@ char 'c'
           ]
        (var "result")
    ]

test2 :: IO ()
test2 = pprint $ module' (Just "Foo") (Just [var "efg"]) []
    [ typeSigs ["efg", "h"] $ tuple [var "A", var "B"]
    , funBind "efg"
        $ match []
        $ rhs (char 'a')
            `where'` [ typeSig "q" $ var "Q"
                     , funBind "q" $ match []
                        $ guardedRhs [var "True" `guard` char 'q']
                     ]
    , funBind "f"
        $ match [var "x", var "y"]
        $ rhs
            (case' (var "y")
                        [matchRhs [wildP] $ var "x"])
            `where'` [funBind "q" $ matchRhs [] $ char 't']
    ]

test3 :: IO ()
test3 = pprint $ module' Nothing Nothing []
    [ funBind "lambdas" $ matchRhs [] $ lambda [var "y"]
                    $ lambdaCase [matchRhs [var "z"] (char 'a')]
    , funBinds "ifs"
        [ matchRhs [var "x"] $ if' (var "b") (var "t") (var "f")
        , matchRhs [var "y"] $ multiIf [guard (var "False") $ char 'f'
                                       , guard (var "True") $ char 't'
                                       ]
        , matchRhs [var "z"] $ multiIf
            [ guard (var "f" @@ var "x") $ string "f"
            , guard (var "g" @@ var "x") $ string "g"
            , guard (var "otherwise") $ string "h"
            ]
        ]
    , funBind "do'"
        $ matchRhs [] (do' [ var "x" <-- var "act"
                        , stmt $ var "return" @@ var "x"
                        ])
    , typeSig "types"
        $ forall' [var "x", var "y"]
        $ [var "Show" @@ var "x"] ==> var "y"
    , typeSig "types'"
        $ [var "Show" @@ var "x"] ==>
            (forall' [var "x", var "y"]
                $ var "y")
    , funBind "swap"
        $ matchRhs [tuple [var "x", var "y"]]
            $ tuple [var "y", var "x"]
    , funBind "char" $ matchRhs [char 'a'] (char 'b')
    , funBind "string" $ matchRhs [string "abc"] (string "def")
    , funBind "as"
        $ matchRhs [asP "x" (tuple [var "y", var "z"])]
            (var "x")
    , funBind "con"
        $ matchRhs [conP "A" [var "b", conP "C" [var "d"]]]
            $ tuple [var "b", var "d"]
    , funBind "ops"
        $ matchRhs [var "x", var "y"]
            $ op (var "x") "+" (var "y")
    , funBinds "ops'"
        [ matchRhs [] (op (int 1) "*"
                        (op (int 2) "+" (int 3)))
        , matchRhs [] (op (var "A" @@ var "x") "*"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z")))
        , matchRhs [] (op (var "A" @@ var "x") "mult"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z")))
        ]
    , funBinds "cons'"
        [ matchRhs [] (var "X" @@ int 1 @@
                        (var "Y" @@ int 2 @@ int 3)
                        @@ var "Z")
        , matchRhs [] (var "f" @@ par (var "g" @@ var "x"))
        ]
    , typeSig "f" $ var "X" @@ var "a" @@
                        (var "Y" @@ var "b" @@ var "c")
                        @@ var "Z"
    , typeSig "g" $ op (var "A" @@ var "x") "*"
                        (op (var "B" @@ var "y") "+"
                                 (var "C" @@ var "z"))
    , class' [var "A" @@ var "a"] "B" ["b", "b'"]
        [ typeSig "f" $ var "b" --> var "b'"
        , funBind "f" $ matchRhs [] $ var "id"
        ]
    , class' [] "F" ["a", "b", "c"]
        [ funDep ["a", "b"] ["c"]
        , funDep ["a"] ["b", "c"]
        ]
    , class' [] "Ident" ["a", "b"]
        [ funDep ["a"] ["b"]
        , funDep ["b"] ["a"]
        , typeSig "ident" $ var "a" --> var "b"
        ]
    , type' "A" ["b", "c"] $ var "D"
    , data' "A" ["b", "c"]
        [ prefixCon "A" [field (var "b"), field (var "c")]
        , prefixCon "D" []
        ]
        [deriving' [var "X", var "Y"]]
    , newtype' "A" ["b", "c"] (prefixCon "A" [field (var "b")])
        [deriving' [var "X", var "Y"]]
    , instance' (var "A" @@ var "b" @@ var "c")
        [ typeSig "f" $ var "b" --> var "c"
        , funBind "f" $ matchRhs [] $ var "undefined"
        ]
    , let a = var "a"
      in class'
           [var "Real" @@ a, var "Enum" @@ a]
           "Integral"
           ["a"]
           [ typeSig "divMod" $ a --> a --> tuple [a, a]
           , typeSig "div" $ a --> a --> a
           , funBind "div"
               $ matchRhs [var "x", var "y"]
                  $ var "fst" @@ (var "divMod" @@ var "x" @@ var "y")
           ]
    , instance' (var "Show" @@ var "Bool")
        [ typeSig "show" $ var "Bool" --> var "String"
        , funBinds "show"
            [ matchRhs [var "True"] $ string "True"
            , matchRhs [var "False"] $ string "False"
            ]
        ]
    , data' "X" ["b"]
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
        $ matchRhs
            [strictP (conP "A" [var "b"]),
             lazyP (conP "A" [var "b"])
            ] (char 'x')
    , typeSig "unit" $ unit --> unit
    , funBind "unit" $ matchRhs [unit] unit
    ]

test4 :: IO ()
test4 = pprint constModule

test5 :: IO ()
test5 = pprint $ module' (Just "M") (Just exports) imports []
  where
    exports = [var "a", var "b"]
    imports = [ qualified' $ import' "A"
              , import' "B" `as'` "C"
              , import' "D" `exposing` [var "d"]
              , import' "E" `hiding` [var "e"]
              ]

constModule :: HsModule'
constModule = module' (Just "Const") (Just [var "const"]) []
    [ typeSig "const" $ a --> b --> a
    , funBind "const" $ matchRhs [wildP, x] x
    ]
  where
    a = var "a"
    b = var "b"
    x = var "x"
