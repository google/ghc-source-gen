{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHC.SourceGen.Name

import Data.List (intercalate)
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests" [testOccName, testRdrName]

testRdrName, testOccName :: TestTree
testRdrName = testGroup "RdrName"
    [ testCase "unqual ident" $ do
        UnqualStr "abc" @=? "abc"
    , testCase "qual ident" $ do
        QualStr "Foo" "abc" @=? "Foo.abc"
    , testCase "hierarchical qual ident" $ do
        QualStr "Foo.Bar" "abc" @=? "Foo.Bar.abc"
    , testCase "unqual op" $ do
        UnqualStr "+-+" @=? "+-+"
    , testCase "qual op" $ do
        QualStr "Foo" "+-+" @=? "Foo.+-+"
    , testCase "hierarchical qual op" $ do
        QualStr "Foo.Bar" "+-+" @=? "Foo.Bar.+-+"
    , testProperty "round tip" $ forAll genRdrName $ \r ->
        fromString (rdrNameStrToString r) === r
    ]

testOccName = testGroup "OccName"
    [ testProperty "constructor" $ forAll genUpperName $ \n ->
        fromString n === OccNameStr Constructor (fromString n)
    , testProperty "value" $ forAll genLowerName $ \n ->
        fromString n === OccNameStr Value (fromString n)
    , testProperty "punctuation" $ forAll genOp $ \n ->
        fromString n === OccNameStr Value (fromString n)
    , testProperty "round-trip" $ forAll genOccName $ \o ->
        fromString (occNameStrToString o) === o
    ]

genUpperName, genLowerName, genOp :: Gen String
genUpperName = (:) <$> genUpper <*> listOf genRest
genLowerName = (:) <$> genLower <*> listOf genRest
genOp = listOf1 $ genPunctuation

genUpper, genLower, genRest, genPunctuation :: Gen Char
genUpper = elements "ABC"
genLower = elements "ab1_'"
genRest = elements "Ab1_'"
genPunctuation = elements ".-+"

genOccName :: Gen OccNameStr
genOccName = oneof
    [ OccNameStr Constructor . fromString <$> genUpperName
    , OccNameStr Value . fromString <$> oneof [genLowerName, genOp]
    ]

genModuleName :: Gen ModuleNameStr
genModuleName = fromString . intercalate "." <$> listOf1 genUpperName

genRdrName :: Gen RdrNameStr
genRdrName = oneof
    [ QualStr <$> genModuleName <*> genOccName
    , UnqualStr <$> genOccName
    ]
