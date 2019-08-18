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

-- | A string identifier referring to a name.
--
-- 'OccNameStr' keeps track of whether it is a "constructor" or "variable"
-- (e.g.: @\"Foo\"@ vs @\"foo\"@, respectively).
--
-- 'OccNameStr' is simililar in purpose to GHC's 'OccName'.  However, unlike
-- 'OccName', 'OccNameStr' does not differentiate between the type or function/value
-- namespaces. Functions in this package that take an 'OccNameStr' as input
-- will internally convert it to the proper namespace.  (This approach
-- makes it easier to implement an 'IsString' instance without the context
-- where a name would be used.)
data OccNameStr = OccNameStr !RawNameSpace !FastString
    deriving (Eq, Ord)

data RawNameSpace = Constructor | Value
    deriving (Eq, Ord)

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
    deriving (Eq, Ord)

instance IsString ModuleNameStr where
    fromString = ModuleNameStr . mkModuleNameFS . fsLit

-- | A string identifier which may be qualified to a particular module.
--
-- 'RdrNameStr' wraps an 'OccNameStr' and thus keeps track of whether it is a
-- "constructor" or "variable" (e.g.: @\"Foo.Bar\"@ vs @\"Foo.bar\"@,
-- respectively).
--
-- 'RdrNameStr' is simililar in purpose to GHC's 'RdrName'.  However, unlike
-- 'RdrName', 'RdrNameStr' does not differentiate between the type or function/value
-- namespaces.  Functions in this package that take a 'RdrNameStr' as input
-- will internally convert it to the proper namespace.  (This approach
-- makes it easier to implement an 'IsString' instance without the context
-- where a name would be used.)
--
-- For example:
--
-- > fromString "A.B.c" == QualStr (fromString "A.B") (fromString "c")
-- > fromString "c" == UnqualStr (fromString "c")
data RdrNameStr = UnqualStr OccNameStr | QualStr ModuleNameStr OccNameStr
    deriving (Eq, Ord)

-- GHC always wraps RdrName in a Located.  (Usually: 'Located (IdP pass)')
-- So for convenience, these functions return a Located-wrapped value.
valueRdrName, typeRdrName :: RdrNameStr -> Located RdrName
valueRdrName (UnqualStr r) = builtLoc $ Unqual $ valueOccName r
valueRdrName (QualStr (ModuleNameStr m) r) = builtLoc $ Qual m $ valueOccName r
typeRdrName (UnqualStr r) = builtLoc $ Unqual $ typeOccName r
typeRdrName (QualStr (ModuleNameStr m) r) = builtLoc $ Qual m $ typeOccName r

-- TODO: operators
instance IsString RdrNameStr where
    -- Split "Foo.Bar.baz" into ("Foo.Bar", "baz")
    fromString f = case span (/= '.') (reverse f) of
        (f', '.':f'') ->
            QualStr (fromString $ reverse f'') (fromString $ reverse f')
        _ -> UnqualStr (fromString f)

-- | A RdrName suitable for an import or export list.
-- E.g.: `import F(a, B)`
-- The 'a' should be a value, but the 'B' should be a type/class.
-- (Currently, GHC doesn't distinguish the class and type namespaces.)
exportRdrName :: RdrNameStr -> Located RdrName
exportRdrName (UnqualStr r) = builtLoc $ Unqual $ exportOccName r
exportRdrName (QualStr (ModuleNameStr m) r) = builtLoc $ Qual m $ exportOccName r

exportOccName :: OccNameStr -> OccName
exportOccName (OccNameStr Value s) = mkVarOccFS s
exportOccName (OccNameStr Constructor s) = mkTcOccFS s
