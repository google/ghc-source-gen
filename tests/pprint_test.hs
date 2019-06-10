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

main :: IO ()
main = runGhc (Just libdir) $ do
    dflags <- getDynFlags
    liftIO $ defaultMain $ testGroup "Tests"
        [typesTest dflags, exprsTest dflags]

typesTest, exprsTest :: DynFlags -> TestTree
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
    ]
  where
    test = testExprs dflags
