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

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import qualified GHC.Driver.Errors as Error (printMessages)
#if MIN_VERSION_ghc(9,6,0)
import qualified GHC.Types.Error as GHC (NoDiagnosticOpts(..))
#endif
#elif MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Errors as Error
import qualified GHC.Parser.Errors.Ppr as Error
#elif MIN_VERSION_ghc(9,0,0)
import qualified GHC.Utils.Error as Error
#endif

#if MIN_VERSION_ghc(9,0,1)
import GHC.Data.FastString
import GHC.Types.Name
    ( Name
    , isExternalName
    , isInternalName
    , isSystemName
    , isWiredInName
    , nameOccName
    , nameUnique
    )
import GHC.Types.Name.Occurrence
    ( OccName
    , occNameSpace
    , occNameString
    , NameSpace
    , varName
    , dataName
    , tvName
    , tcClsName
    )

import qualified GHC.Driver.Session as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC as GHC
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser as Parser
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Data.StringBuffer as GHC
import GHC.Paths (libdir)
import GHC.Driver.Monad (liftIO)
#else
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
import qualified Lexer as GHC
import qualified Parser as Parser
import qualified SrcLoc as GHC
import qualified StringBuffer as GHC
import GHC.Paths (libdir)
#if MIN_VERSION_ghc(8,10,0)
import GhcMonad (liftIO)
import qualified ErrUtils as Error
#else
import qualified Outputable as GHC
#endif
#endif

#if MIN_VERSION_ghc(8,10,0)
import System.Exit (exitFailure)
#endif

main :: IO ()
main = do
    [f] <- getArgs
    result <- parseModule f
    print $ gPrint result

#if MIN_VERSION_ghc(9,0,1) && !MIN_VERSION_ghc(9,6,0)
parseModule :: FilePath -> IO GHC.HsModule
#else
parseModule :: FilePath -> IO (GHC.HsModule GHC.GhcPs)
#endif
parseModule f = GHC.runGhc (Just libdir) $ do
    dflags <- GHC.getDynFlags
    contents <- GHC.liftIO $ GHC.stringToStringBuffer <$> readFile f
#if MIN_VERSION_ghc(9,4,0)
    let (_, options) = GHC.getOptions (initParserOpts dflags) contents f
#else
    let options = GHC.getOptions dflags contents f
#endif
    (dflags', _, _) <- GHC.parseDynamicFilePragma dflags options

#if MIN_VERSION_ghc(9,4,0)
    let diagOpts = initDiagOpts dflags'
        state =
            GHC.initParserState
                ( GHC.mkParserOpts
                    (GHC.extensionFlags dflags')
                    diagOpts
                    []
                    (GHC.safeImportsOn dflags')
                    (GHC.gopt GHC.Opt_Haddock dflags')
                    (GHC.gopt GHC.Opt_KeepRawTokenStream dflags')
                    True
                )
#elif MIN_VERSION_ghc(9,2,0)
    let state =
            GHC.initParserState
                ( GHC.mkParserOpts
                    (GHC.warningFlags dflags')
                    (GHC.extensionFlags dflags')
                    (GHC.safeImportsOn dflags')
                    (GHC.gopt GHC.Opt_Haddock dflags')
                    (GHC.gopt GHC.Opt_KeepRawTokenStream dflags')
                    True
                )
#else
    let state =
            GHC.mkPState
                dflags'
#endif
                contents
                (GHC.mkRealSrcLoc (GHC.fsLit f) 1 1)

    case GHC.unP Parser.parseModule state of
        GHC.POk _state m -> return $ GHC.unLoc m
#if MIN_VERSION_ghc(9,4,0)
        GHC.PFailed s -> do
            logger <- GHC.getLogger
            liftIO $ do
                let errors = GHC.getPsErrorMessages s
#  if MIN_VERSION_ghc(9,6,0)
                Error.printMessages logger GHC.NoDiagnosticOpts diagOpts errors
#  else
                Error.printMessages logger diagOpts errors
#  endif
                exitFailure
#elif MIN_VERSION_ghc(9,2,0)
        GHC.PFailed s -> do
            logger <- GHC.getLogger
            liftIO $ do
                let errors = Error.pprError <$> GHC.getErrorMessages s
                Error.printBagOfErrors logger dflags errors
                exitFailure
#elif MIN_VERSION_ghc(8,10,0)
        GHC.PFailed s -> liftIO $ do
                let (_warnings, errors) = GHC.messages s dflags
                Error.printBagOfErrors dflags errors
                exitFailure
#else
        GHC.PFailed
            -- Note: using printBagOfErrors on the messages doesn't produce any
            -- useful output on older GHCs; so instead print the docs directly.
            _messages
            loc docs ->
            error $ GHC.showPpr dflags loc ++ ": " ++ GHC.showSDoc dflags docs
#endif

gPrint :: Data a => a -> Doc
gPrint x
    | showConstr c == "L", [_,e] <- xs = e
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
