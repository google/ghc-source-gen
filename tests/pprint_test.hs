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
import GhcVersion

data TestCase a = String :~ a

infixr 0 :~

testCases :: Outputable a => DynFlags -> String -> [TestCase a] -> TestTree
testCases dflags name cases = testGroup name $ map run cases
  where
    run (expected :~ x) =
        testCase (takeWhile (/='\n') expected) $ expected @=? showPpr dflags x

testTypes :: DynFlags ->  String -> [TestCase HsType'] -> TestTree
testTypes = testCases

testExprs :: DynFlags ->  String -> [TestCase HsExpr'] -> TestTree
testExprs = testCases

testDecls :: DynFlags ->  String -> [TestCase HsDecl'] -> TestTree
testDecls = testCases

testPats :: DynFlags ->  String -> [TestCase Pat'] -> TestTree
testPats = testCases


main :: IO ()
main = runGhc (Just libdir) $ do
    dflags <- getDynFlags
    liftIO $ defaultMain $ testGroup "Tests"
        [typesTest dflags, exprsTest dflags, declsTest dflags, patsTest dflags]

typesTest, exprsTest, declsTest, patsTest :: DynFlags -> TestTree
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
        , "A (B x)" :~ var "A" @@ par (var "B" @@ var "x")
        , "A ((B x))" :~ var "A" @@ par (par (var "B" @@ var "x"))
        , "A x (B y z)" :~ var "A" @@ var "x" @@ (var "B" @@ var "y" @@ var "z")
        , "A w (B x y) Z"
            :~ var "A" @@ var "w" @@ (var "B" @@ var "x" @@ var "y") @@ var "Z"
        ]
    , test "op"
        [ "x + y" :~ op (var "x") "+" (var "y")
        , "x `add` y" :~ op (var "x") "add" (var "y")
        , "x * (y + z)" :~ op (var "x") "*" (op (var "y") "+" (var "z"))
        , "(x * y) + z" :~ op (op (var "x") "*" (var "y")) "+" (var "z")
        , "x `mult` (y `add` z)" :~ op (var "x") "mult" (op (var "y") "add" (var "z"))
        , "A x * (B y + C z)" :~ op (var "A" @@ var "x") "*"
                                    (op (var "B" @@ var "y") "+" (var "C" @@ var "z"))
        , "(f . g) x" :~ op (var "f") "." (var "g") @@ var "x"
        ]
    , test "function"
        [ "a -> b" :~ var "a" --> var "b"
        , "a -> b -> c" :~ var "a" --> var "b" --> var "c"
        , "a -> b -> c" :~ var "a" --> (var "b" --> var "c")
        , "(a -> b) -> c" :~ (var "a" --> var "b") --> var "c"
        -- These tests also check that ==> and --> have compatible precedences:
        , "A a => a -> b" :~ [var "A" @@ var "a"] ==> var "a" --> var "b"
        , "(A a, B b) => a -> b" :~
            [var "A" @@ var "a", var "B" @@ var "b"] ==> var "a" --> var "b"
        -- It appears to be correct to *not* wrap `A => c` in parentheses;
        -- GHC still parses it as a function between two HsQualTy.
        , "(A => b) -> A => c" :~
            ([var "A"] ==> var "b") --> ([var "A"] ==> var "c")
        , "(A => b) -> A => c" :~
            ([var "A"] ==> var "b") --> [var "A"] ==> var "c"
        ]
    , test "literals"
        [ "\"abc\"" :~ stringTy "abc"
        , "123" :~ numTy 123
        ]
    , test "unit"
        [ "()" :~ unit ]
   , test "list"
        [ "[x]" :~ listTy (var "x")
        , "'[]" :~ listPromotedTy []
        , "'[x]" :~ listPromotedTy [var "x"]
        , "'[y, z]" :~ listPromotedTy [var "y", var "z"]
        ]
    , test "tyPromotedVar"
        -- For some reason, older GHC pretty-printed an extra space.
        [ ifGhc88 "'Abc" " 'Abc" :~ tyPromotedVar "Abc"
        , ifGhc88 "T 'Abc" "T  'Abc" :~ var "T" @@ tyPromotedVar "Abc"
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
        , "(Prelude.+) x" :~ var "Prelude.+" @@ var "x"
        , "A (B x)" :~ var "A" @@ (var "B" @@ var "x")
        , "A (B x)" :~ var "A" @@ par (var "B" @@ var "x")
        , "A ((B x))" :~ var "A" @@ par (par (var "B" @@ var "x"))
        , "A x (B y z)" :~ var "A" @@ var "x" @@ (var "B" @@ var "y" @@ var "z")
        , "A w (B x y) Z"
            :~ var "A" @@ var "w" @@ (var "B" @@ var "x" @@ var "y") @@ var "Z"
        , "A 3" :~ var "A" @@ int 3
        , "A (-3)" :~ var "A" @@ int (-3)
        , "A 3.0" :~ var "A" @@ frac 3.0
        , "A (-3.0)" :~ var "A" @@ frac (-3.0)
        , "A 'x'" :~ var "A" @@ char 'x'
        , "A \"xyz\"" :~ var "A" @@ string "xyz"
        , "(\\ x -> x) (\\ x -> x)" :~
            let f = lambda [bvar "x"] (var "x")
            in f @@ f
        , "f x @t" :~ tyApp (var "f" @@ var "x") (var "t")
        , "f (x @t)" :~ var "f" @@ (tyApp (var "x") (var "t"))
        ]
    , test "op"
        [ "x + y" :~ op (var "x") "+" (var "y")
        , "x Prelude.+ y" :~ op (var "x") "Prelude.+" (var "y")
        , "x `add` y" :~ op (var "x") "add" (var "y")
        , "x * (y + z)" :~ op (var "x") "*" (op (var "y") "+" (var "z"))
        , "(x * y) + z" :~ op (op (var "x") "*" (var "y")) "+" (var "z")
        , "x `mult` (y `add` z)" :~ op (var "x") "mult" (op (var "y") "add" (var "z"))
        , "A x * (B y + C z)" :~ op (var "A" @@ var "x") "*"
                                    (op (var "B" @@ var "y") "+" (var "C" @@ var "z"))
        , "(f . g) x" :~ op (var "f") "." (var "g") @@ var "x"
        , "(\\ x -> x) . (\\ x -> x)" :~
            let f = lambda [bvar "x"] (var "x")
            in op f "." f
        , "x @s + y @t" :~
                op (var "x" `tyApp` var "s") "+" (var "y" `tyApp` var "t")
        ]
    , test "period-op"
        [ "(Prelude..) x" :~ var "Prelude.." @@ var "x"
        , "x Prelude.. y" :~ op (var "x") "Prelude.." (var "y")
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
        , "f x @t" :~ (var "f" @@ var "x") `tyApp` var "t"
        , "f (x @t)" :~ var "f" @@ (var "x" `tyApp` var "t")
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
    , test "let"
        [ "let x = 1 in x" :~ let' [valBind "x" $ int 1] (var "x")
        , "let f x = 1 in f" :~
            let' [ funBind "f" $ match [bvar "x"] $ int 1] (var "f")
        , "let f (A x) = 1 in f" :~
            let' [ funBind "f" $ match [conP "A" [bvar "x"]] $ int 1] (var "f")
        ]
    , test "do"
        -- TODO: add more tests.
        [ "do (let x = 1 in x)" :~ do' [stmt $ let' [valBind "x" (int 1)] (var "x")]
        ]
    ]
  where
    test = testExprs dflags

declsTest dflags = testGroup "Decls"
    [ test "patBind"
        [ "x = x" :~ patBind (bvar "x") (var "x")
        , "(x, y) = (y, x)" :~ patBind (tuple [bvar "x", bvar "y"])
                                    (tuple [var "y", var "x"])
        , "(x, y)\n  | test = (1, 2)\n  | otherwise = (2, 3)" :~
            patBindGRHSs (tuple [bvar "x", bvar "y"])
                $ guardedRhs
                    [ var "test" `guard` tuple [int 1, int 2]
                        , var "otherwise" `guard` tuple [int 2, int 3]
                    ]
        , "z | Just y <- x, y = ()" :~
            patBindGRHSs (bvar "z")
                $ guardedRhs
                    [guards
                        [ conP "Just" [bvar "y"] <-- var "x"
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
        , "x = (+)" :~ valBind "x" $ var "+"
        ]
    , test "funBind"
        [ "not True = False\nnot False = True" :~
             funBinds "not"
                [ match [bvar "True"] (var "False")
                , match [bvar "False"] (var "True")
                ]
        , "not x\n  | x = False\n  | otherwise = True" :~
            funBind "not"
                $ matchGRHSs [bvar "x"] $ guardedRhs
                    [ guard (var "x") (var "False")
                    , guard (var "otherwise") (var "True")
                    ]
        , "f (A x) = 1" :~ funBind "f" $ match [conP "A" [bvar "x"]] (int 1)
        ]
    , test "tyFamInst"
        [ "type instance Elt String = Char"
            :~ tyFamInst "Elt" [var "String"] (var "Char")
        , "instance Container String where\n  type Elt String = Char"
            :~ instance' (var "Container" @@ var "String")
                [tyFamInst "Elt" [var "String"] (var "Char")]
        ]
    , test "patSynSigs"
        [ "pattern F, G :: T" :~ patSynSigs ["F", "G"] $ var "T"
        , "pattern F :: T" :~ patSynSig "F" $ var "T"
        ]
    , test "patSynBind"
        [ "pattern F a b = G b a"
            :~ patSynBind "F" ["a", "b"] $ conP "G" [bvar "b", bvar "a"]
        ]
    ]
  where
    test = testDecls dflags

patsTest dflags = testGroup "Pats"
    [ test "app"
        [ "A x y" :~ conP "A" [bvar "x", bvar "y"]
        , "(:) x y" :~ conP ":" [bvar "x", bvar "y"]
        , "(Prelude.:) x" :~ conP "Prelude.:" [bvar "x"]
        , "A (B x)" :~ conP "A" [conP "B" [bvar "x"]]
        , "A (B x)" :~ conP "A" [par $ conP "B" [bvar "x"]]
        , "A ((B x))" :~ conP "A" [par $ par $ conP "B" [bvar "x"]]
        , "A x (B y z)" :~ conP "A" [bvar "x", conP "B" [bvar "y", bvar "z"]]
        , "A w (B x y) Z"
            :~ conP "A" [bvar "w", conP "B" [bvar "x", bvar "y"], conP "Z" []]
        , "A 3" :~ conP "A" [int 3]
        , "A (-3)" :~ conP "A" [int (-3)]
        , "A 3.0" :~ conP "A" [frac 3.0]
        , "A (-3.0)" :~ conP "A" [frac (-3.0)]
        , "A 'x'" :~ conP "A" [char 'x']
        , "A \"xyz\"" :~ conP "A" [string "xyz"]
        , "A B {x = C}"
            :~ conP "A" [recordConP "B" [("x", conP "C" [])]]
        ]
    , test "asP"
        [ "x@B" :~ asP "x" $ conP "B" []
        , "x@(B y)" :~ asP "x" $ conP "B" [bvar "y"]
        , "x@_" :~ asP "x" wildP
        ]
    , test "strictP"
        [ "!x" :~ strictP $ bvar "x"
        , "!B" :~ strictP $ conP "B" []
        , "!(B y)" :~ strictP $ conP "B" [bvar "y"]
        , "!_" :~ strictP wildP
        ]
    , test "lazyP"
        [ "~x" :~ lazyP $ bvar "x"
        , "~B" :~ lazyP $ conP "B" []
        , "~(B y)" :~ lazyP $ conP "B" [bvar "y"]
        , "~_" :~ lazyP wildP
        ]
    , test "sigPat"
        [ "x :: A" :~ sigP (bvar "x") (bvar "A")
        , "A x :: A x" :~ sigP (conP "A" [bvar "x"]) (bvar "A" @@ bvar "x")
        ]
    , test "recordConP"
        [ "A {x = Y}" :~ recordConP "A" [("x", conP "Y" [])]
        ]
    ]
  where
    test = testPats dflags

