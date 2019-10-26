-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Data (Data, showConstr,  toConstr, gmapQ)
import Data.Typeable (cast)
import System.Environment (getArgs)
import Text.PrettyPrint

import FastString
import Name
    ( Name
    , isExternalName
    , isInternalName
    , isSystemName
    , isWiredInName
    , nameOccName
    , nameUnique
    )
import OccName
    ( OccName
    , occNameSpace
    , occNameString
    , NameSpace
    , varName
    , dataName
    , tvName
    , tcClsName
    )

import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified GHC as GHC
import qualified GhcMonad as GHC
import qualified HeaderInfo as GHC
import qualified Outputable as GHC
import qualified Lexer as GHC
import qualified Parser as Parser
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC
import GHC.Paths (libdir)

main :: IO ()
main = do
    [f] <- getArgs
    result <- parseModule f
    print $ gPrint result

#if MIN_VERSION_ghc(8,4,0)
parseModule :: FilePath -> IO (GHC.HsModule GHC.GhcPs)
#else
parseModule :: FilePath -> IO (GHC.HsModule GHC.RdrName)
#endif
parseModule f = GHC.runGhc (Just libdir) $ do
    dflags <- GHC.getDynFlags
    contents <- GHC.liftIO $ GHC.stringToStringBuffer <$> readFile f
    let options = GHC.getOptions dflags contents f
    (dflags', _, _) <- GHC.parseDynamicFilePragma dflags options
    let state = GHC.mkPState dflags' contents (GHC.mkRealSrcLoc (GHC.fsLit f) 1 1)
    case GHC.unP Parser.parseModule state of
        GHC.POk _state m -> return $ GHC.unLoc m
        GHC.PFailed
#if MIN_VERSION_ghc(8,4,0)
            _message
#endif
            loc docs ->
            error $ GHC.showPpr dflags loc ++ GHC.showSDoc dflags docs

gPrint :: Data a => a -> Doc
gPrint x
    -- | showConstr c == "L", [_,e] <- xs = e
    | showConstr c == "(:)" = gPrintList x
    | Just occ <- cast x = text $ showOccName occ
    | Just name <- cast x = text $ showName name
    | Just s <- cast x = text $ showFastString s
    | otherwise =
        hang (text $ showConstr c) 2 (sep $ map parens xs)
  where
    c = toConstr x
    xs = gmapQ gPrint x

gPrintList :: Data a => a -> Doc
gPrintList = brackets . sep . punctuate comma . elems
  where
    elems :: Data b => b -> [Doc]
    elems xs = case gmapQ SomeData xs of
                [] -> []
                [x,y] -> renderCons x y
                _ -> error $ "gPrintList: unexpected number of fields"
    renderCons :: SomeData -> SomeData -> [Doc]
    renderCons (SomeData x) (SomeData y) = gPrint x : elems y

data SomeData where
    SomeData :: Data a => a -> SomeData

showOccName :: OccName -> String
showOccName o = "OccName{" ++ showNameSpace (occNameSpace o)
                ++ "," ++ show (occNameString o) ++ "}"

showFastString :: FastString -> String
showFastString = show . unpackFS

showNameSpace :: NameSpace -> String
showNameSpace ns
    | ns == varName = "VarName"
    | ns == dataName = "DataName"
    | ns == tvName = "TvName"
    | ns == tcClsName = "TcClsName"
    | otherwise = "Unknown"

showName :: Name -> String
showName n = "Name{" ++ nameSort ++ ":" ++ showOccName (nameOccName n)
                ++ "," ++ show (nameUnique n)
                ++ "}"
  where
    nameSort
        | isExternalName n = "external"
        | isInternalName n = "internal"
        | isSystemName n = "system"
        | isWiredInName n = "wired-in"
        | otherwise = "unknown" -- Shouldn't happen; these guards are exhaustive
