{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import DynFlags (getDynFlags)
import GhcMonad (liftIO)
import GHC.Paths (libdir)
import GHC (runGhc, DynFlags)
import Outputable (Outputable)

import Test.Tasty
import Test.Tasty.HUnit

import GHC.SourceGen

data TestCase a = String :~ a

infixr 0 :~

testCases :: Outputable a => DynFlags -> String -> [TestCase a] -> TestTree
testCases dflags name cases = testCase name $ mapM_ run cases
  where
    run (expected :~ x) = expected @=? showPpr dflags x

testTypes :: DynFlags ->  String -> [TestCase HsType'] -> TestTree
testTypes = testCases

testExprs :: DynFlags ->  String -> [TestCase HsExpr'] -> TestTree
testExprs = testCases

testDecls :: DynFlags ->  String -> [TestCase HsDecl'] -> TestTree
testDecls = testCases

main :: IO ()
main = runGhc (Just libdir) $ do
    dflags <- getDynFlags
    liftIO $ defaultMain $ testGroup "Tests"
        [typesTest dflags, exprsTest dflags, declsTest dflags]

typesTest, exprsTest, declsTest :: DynFlags -> TestTree
typesTest dflags = testGroup "Type"
    [ test "var"
        [ "A" :~ var "A"
        , "x" :~ var "x"
        , "A.x" :~ var "A.x"
        , "x" :~ var (unqual "x")
        , "A.x" :~ var (qual "A" "x")
        ]
    , test "app"
        [ "A x" :~ var "A" @@ var "x"
        , "(+) x" :~ var "+" @@ var "x"
        , "A (B x)" :~ var "A" @@ par (var "B" @@ var "x")
        , "A x (B y z)" :~ var "A" @@ var "x" @@ (var "B" @@ var "y" @@ var "z")
        , "A w (B x y) Z"
            :~ var "A" @@ var "w" @@ (var "B" @@ var "x" @@ var "y") @@ var "Z"
        ]
    , test "op"
        [ "x + y" :~ op (var "x") "+" (var "y")
        , "x `add` y" :~ op (var "x") "add" (var "y")
        , "x * (y + z)" :~ op (var "x") "*" (op (var "y") "+" (var "z"))
        , "x `mult` (y `add` z)" :~ op (var "x") "mult" (op (var "y") "add" (var "z"))
        , "A x * (B y + C z)" :~ op (var "A" @@ var "x") "*"
                                    (op (var "B" @@ var "y") "+" (var "C" @@ var "z"))
        ]
    , test "function"
        [ "a -> b" :~ var "a" --> var "b"
        , "a -> b -> c" :~ var "a" --> var "b" --> var "c"
        , "a -> b -> c" :~ var "a" --> (var "b" --> var "c")
        , "(a -> b) -> c" :~ (var "a" --> var "b") --> var "c"
        ]
    , test "literals"
        [ "\"abc\"" :~ stringTy "abc"
        , "123" :~ numTy 123
        ]
    , test "unit"
        [ "()" :~ unit ]
   , test "list"
        [ "[x]" :~ listTy (var "x")
        , "[]" :~ listPromotedTy []
        , "[x]" :~ listPromotedTy [var "x"]
        , "[y, z]" :~ listPromotedTy [var "y", var "z"]
        ]
    ]
  where
    test = testTypes dflags

exprsTest dflags = testGroup "Expr"
    [ test "var"
        [ "A" :~ var "A"
        , "x" :~ var "x"
        , "A.x" :~ var "A.x"
        , "x" :~ var (unqual "x")
        , "A.x" :~ var (qual "A" "x")
        ]
    , test "app"
        [ "A x" :~ var "A" @@ var "x"
        , "(+) x" :~ var "+" @@ var "x"
        , "A (B x)" :~ var "A" @@ par (var "B" @@ var "x")
        , "A x (B y z)" :~ var "A" @@ var "x" @@ (var "B" @@ var "y" @@ var "z")
        , "A w (B x y) Z"
            :~ var "A" @@ var "w" @@ (var "B" @@ var "x" @@ var "y") @@ var "Z"
        , "A 3" :~ var "A" @@ int 3
        , "A (-3)" :~ var "A" @@ int (-3)
        , "A (3 % 1)" :~ var "A" @@ frac 3.0
        , "A ((-3) % 1)" :~ var "A" @@ frac (-3.0)
        , "A 'x'" :~ var "A" @@ char 'x'
        , "A \"xyz\"" :~ var "A" @@ string "xyz"
        ]
    , test "op"
        [ "x + y" :~ op (var "x") "+" (var "y")
        , "x `add` y" :~ op (var "x") "add" (var "y")
        , "x * (y + z)" :~ op (var "x") "*" (op (var "y") "+" (var "z"))
        , "x `mult` (y `add` z)" :~ op (var "x") "mult" (op (var "y") "add" (var "z"))
        , "A x * (B y + C z)" :~ op (var "A" @@ var "x") "*"
                                    (op (var "B" @@ var "y") "+" (var "C" @@ var "z"))
        ]
    , test ":@@:"
        -- TODO: GHC puts extra space here.
        [ "  e :: t" :~ var "e" @::@ var "t" ]
    , test "unit"
        [ "()" :~ unit ]
    , test "list"
        [ "[]" :~ list []
        , "[]" :~ nil
        , "[x]" :~ list [var "x"]
        , "[y, z]" :~ list [var "y", var "z"]
        , "(:)" :~ cons
        , "(:) x y" :~ cons @@ var "x" @@ var "y"
        ]
    , test "tyApp"
        [ "x @a" :~ tyApp (var "x") (var "a")
        , "x @a @b" :~ tyApp (tyApp (var "x") (var "a")) (var "b")
        , "x @a b" :~ tyApp (var "x") (var "a") @@ var "b"
        , "x @(a b)" :~ tyApp (var "x") (var "a" @@ var "b")
        , "x @(a + b)" :~ tyApp (var "x") (op (var "a") "+" (var "b"))
        ]
    , test "recordConE"
        [ "A {}" :~ recordConE "A" []
        , "A {x = 1, y = 2}" :~ recordConE "A" [("x", int 1), ("y", int 2)]
        ]
    , test "recordUpd"
        [ "r {b = x, c = y}"
            :~ recordUpd (var "r") [("b", var "x"), ("c", var "y")]
        , "(f x) {b = x}"
            :~ recordUpd (var "f" @@ var "x") [("b", var "x")]
        , "f x {b = x}"
            :~ var "f" @@ recordUpd (var "x") [("b", var "x")]
        , "(x + y) {b = x}"
            :~ recordUpd (op (var "x") "+" (var "y")) [("b", var "x")]
        , "x + y {b = x}"
            :~ op (var "x") "+" (recordUpd (var "y") [("b", var "x")])
        ]
    ]
  where
    test = testExprs dflags

declsTest dflags = testGroup "Decls"
    [ test "patBind"
        [ "x = x" :~ patBind (var "x") (var "x")
        , "(x, y) = (y, x)" :~ patBind (tuple [var "x", var "y"])
                                    (tuple [var "y", var "x"])
        , "(x, y)\n  | test = (1, 2)\n  | otherwise = (2, 3)" :~
            patBindGRHSs (tuple [var "x", var "y"])
                $ guardedRhs
                    [ var "test" `guard` tuple [int 1, int 2]
                        , var "otherwise" `guard` tuple [int 2, int 3]
                    ]
        , "z | Just y <- x, y = ()" :~
            patBindGRHSs (var "z")
                $ guardedRhs
                    [guards
                        [ conP "Just" [var "y"] <-- var "x"
                        , stmt (var "y")
                        ]
                        unit
                    ]
        ]
    , test "valBind"
        [ "x = y" :~ valBindGRHSs "x" $ rhs $ var "y"
        , "x = y" :~ valBind "x" $ var "y"
        , "x | test = 1\n  | otherwise = 2" :~
            valBindGRHSs "x"
            $ guardedRhs
                [ var "test" `guard` int 1
                , var "otherwise" `guard` int 2
                ]
        ]
    , test "funBind"
        [ "not True = False\nnot False = True" :~
             funBinds "not"
                [ match [var "True"] (var "False")
                , match [var "False"] (var "True")
                ]
        , "not x\n  | x = False\n  | otherwise = True" :~
            funBind "not"
                $ matchGRHSs [var "x"] $ guardedRhs
                    [ guard (var "x") (var "False")
                    , guard (var "otherwise") (var "True")
                    ]
        ]
    , test "tyFamInst"
        [ "type instance Elt String = Char"
            :~ tyFamInst "Elt" [var "String"] (var "Char")
        , "instance Container String where\n  type Elt String = Char"
            :~ instance' (var "Container" @@ var "String")
                [tyFamInst "Elt" [var "String"] (var "Char")]
        ]
    ]
  where
    test = testDecls dflags
