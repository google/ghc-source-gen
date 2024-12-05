-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
module GHC.SourceGen.Name.Internal where

import Data.Char (isAlphaNum, isUpper)
import Data.Function (on)
import Data.List (intercalate)
import Data.String (IsString(..))
import GHC.Data.FastString (FastString, fsLit)
import GHC.Unit.Module (mkModuleNameFS, ModuleName, moduleNameString)
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader

#if MIN_VERSION_ghc(9,2,0)
import GHC.Data.FastString (LexicalFastString(..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation (LocatedN)
#else
import GHC.Types.SrcLoc (Located)
#endif

import GHC.SourceGen.Syntax.Internal (mkLocated)

-- | A string identifier referring to a name.
--
-- 'OccNameStr' keeps track of whether it is a "constructor" or "variable"
-- (e.g.: @\"Foo\"@ vs @\"foo\"@, respectively).
--
-- 'OccNameStr' is simililar in purpose to GHC's 'OccName'.  However, unlike
-- 'OccName', 'OccNameStr' does not differentiate between the namespace
-- of types and of values.
-- Functions in this package that take an 'OccNameStr' as input
-- will internally convert it to the proper namespace.  (This approach
-- makes it easier to implement an 'IsString' instance without the context
-- where a name would be used.)
data OccNameStr = OccNameStr !RawNameSpace !FastString
    deriving (Show, Eq)

instance Ord OccNameStr where
    compare = compare `on` (\(OccNameStr n s) -> (n, fromFastString s))
      where
#if MIN_VERSION_ghc(9,2,0)
        fromFastString = LexicalFastString
#else
        fromFastString = id
#endif

data RawNameSpace = Constructor | Value
    deriving (Show, Eq, Ord)

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

instance Show ModuleNameStr where
    show = show . moduleNameString . unModuleNameStr

instance IsString ModuleNameStr where
    fromString = ModuleNameStr . mkModuleNameFS . fsLit

-- | A string identifier which may be qualified to a particular module.
--
-- 'RdrNameStr' wraps an 'OccNameStr' and thus keeps track of whether it is a
-- "constructor" or "variable" (e.g.: @\"Foo.Bar\"@ vs @\"Foo.bar\"@,
-- respectively).
--
-- 'RdrNameStr' is simililar in purpose to GHC's 'RdrName'.  However, unlike
-- 'RdrName', 'RdrNameStr' does not differentiate between the namespace of types
-- and of values.
-- Functions in this package that take a 'RdrNameStr' as input
-- will internally convert it to the proper namespace.  (This approach
-- makes it easier to implement an 'IsString' instance without the context
-- where a name would be used.)
--
-- For example:
--
-- > fromString "A.B.c" == QualStr (fromString "A.B") (fromString "c")
-- > fromString "c" == UnqualStr (fromString "c")
data RdrNameStr = UnqualStr OccNameStr | QualStr ModuleNameStr OccNameStr
    deriving (Show, Eq, Ord)

#if !MIN_VERSION_ghc(9,2,0)
type LocatedN e = Located e
#endif

-- GHC always wraps RdrName in a Located.  (Usually: 'Located (IdP pass)')
-- So for convenience, these functions return a Located-wrapped value.
valueRdrName, typeRdrName :: RdrNameStr -> LocatedN RdrName
valueRdrName (UnqualStr r) = mkLocated $ Unqual $ valueOccName r
valueRdrName (QualStr (ModuleNameStr m) r) = mkLocated $ Qual m $ valueOccName r
typeRdrName (UnqualStr r) = mkLocated $ Unqual $ typeOccName r
typeRdrName (QualStr (ModuleNameStr m) r) = mkLocated $ Qual m $ typeOccName r

-- TODO: operators
instance IsString RdrNameStr where
    -- Split "Foo.Bar.baz" into ("Foo.Bar", "baz")
    fromString s = case collectModuleName s of
        (m, n)
            | null m -> UnqualStr (fromString n)
            | otherwise -> QualStr (fromString $ intercalate "." m) (fromString n)

collectModuleName :: String -> ([String],String)
collectModuleName s = case span isVarChar s of
    ("", n) -> ([], n)  -- Symbol
    (n, "") -> ([], n)  -- Identifier
    (m, '.' : s') -> case collectModuleName s' of
                            (m', s'') -> (m : m', s'')
    _ -> error $ "Unable to parse RdrNameStr: " ++ show s
  where
    isVarChar '\'' = True
    isVarChar '_' = True
    isVarChar c = isAlphaNum c

-- | A RdrName suitable for an import or export list.
-- E.g.: `import F(a, B)`
-- The 'a' should be a value, but the 'B' should be a type/class.
-- (Currently, GHC doesn't distinguish the class and type namespaces.)
exportRdrName :: RdrNameStr -> LocatedN RdrName
exportRdrName (UnqualStr r) = mkLocated $ Unqual $ exportOccName r
exportRdrName (QualStr (ModuleNameStr m) r) = mkLocated $ Qual m $ exportOccName r

exportOccName :: OccNameStr -> OccName
exportOccName (OccNameStr Value s) = mkVarOccFS s
exportOccName (OccNameStr Constructor s) = mkTcOccFS s
