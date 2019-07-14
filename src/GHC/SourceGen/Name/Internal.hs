-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

module GHC.SourceGen.Name.Internal where

import Data.Char (isUpper)
import Data.String (IsString(..))
import FastString (FastString, fsLit)
import Module (mkModuleNameFS, ModuleName)
import RdrName
import OccName
import SrcLoc (Located)

import GHC.SourceGen.Syntax.Internal (builtLoc)

-- | A string identifier.  This definition is simililar to 'RdrName', but
-- independent of whether it's in the type or value namespace.
data OccNameStr = OccNameStr !RawNameSpace !FastString

data RawNameSpace = Constructor | Value
-- TODO: symbols

rawNameSpace :: String -> RawNameSpace
rawNameSpace (c:_)
    | isUpper c = Constructor
rawNameSpace _ = Value

instance IsString OccNameStr where
    fromString s = OccNameStr (rawNameSpace s) (fsLit s)

valueOccName, typeOccName :: OccNameStr -> OccName
valueOccName (OccNameStr Constructor s) = mkDataOccFS s
valueOccName (OccNameStr Value s) = mkVarOccFS s
typeOccName (OccNameStr Constructor s) = mkTcOccFS s
typeOccName (OccNameStr Value s) = mkTyVarOccFS s

-- | A newtype wrapper around 'ModuleName' which is an instance of 'IsString'.
newtype ModuleNameStr = ModuleNameStr { unModuleNameStr :: ModuleName }

instance IsString ModuleNameStr where
    fromString = ModuleNameStr . mkModuleNameFS . fsLit

-- | A string identifier which may be qualified to a particular module.
--
-- For example:
--
-- > fromString "A.b.c" == RawQual (fromString "A.b") (fromString "c")
-- > fromString "c" == RawUnqual (fromString "c")
--
-- This definition is simililar to 'RdrName', but independent of whether it's
-- in the type or value namespace.  Functions in this package that take
-- a 'RdrName' as input will internally convert it to the proper namespace.
data RdrNameStr = RawUnqual OccNameStr | RawQual ModuleNameStr OccNameStr

-- GHC always wraps RdrName in a Located.  (Usually: 'Located (IdP pass)')
-- So for convenience, these functions return a Located-wrapped value.
valueRdrName, typeRdrName :: RdrNameStr -> Located RdrName
valueRdrName (RawUnqual r) = builtLoc $ Unqual $ valueOccName r
valueRdrName (RawQual (ModuleNameStr m) r) = builtLoc $ Qual m $ valueOccName r
typeRdrName (RawUnqual r) = builtLoc $ Unqual $ typeOccName r
typeRdrName (RawQual (ModuleNameStr m) r) = builtLoc $ Qual m $ typeOccName r

-- TODO: operators
instance IsString RdrNameStr where
    -- Split "Foo.Bar.baz" into ("Foo.Bar", "baz")
    fromString f = case span (/= '.') (reverse f) of
        (f', '.':f'') ->
            RawQual (fromString $ reverse f'') (fromString $ reverse f')
        _ -> RawUnqual (fromString f)
