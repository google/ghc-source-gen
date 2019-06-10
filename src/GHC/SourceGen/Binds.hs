-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell declarations.
module GHC.SourceGen.Binds
    ( -- * Overloaded constructors
      HasValBind(..)
    , typeSig
    , typeSigs
    , funBind
    , funBinds
    -- * RawMatch
    -- $rawMatch
    , RawMatch
    , match
    , matchRhs
    -- ** RawGRHS
    , RawGRHS
    , rhs
    , guardedStmt
    , guarded
    -- ** Statements
    , stmt
    , (<--)
    -- ** Where clauses
    , where'
    , RawValBind
    ) where

import BasicTypes (LexicalFixity(..))
import HsBinds
import HsExpr
import HsDecls
import HsTypes
import TcEvidence (HsWrapper(WpHole))

import GHC.SourceGen.Binds.Internal
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal (sigWcType)

-- | Declare that a multiple functions or values have a type:
--
-- > f, g :: A
-- > =====
-- > typeSigs ["f", "g"] (var "A")
typeSigs :: HasValBind t => [RawRdrName] -> HsType' -> t
typeSigs names t =
    sigB $ noExt TypeSig (map typeRdrName names)
        $ sigWcType t

-- | Declare that a function or value has a type:
--
-- > f :: A
-- > =====
-- > typeSig "f" (var "A")
typeSig :: HasValBind t => RawRdrName -> HsType' -> t
typeSig n = typeSigs [n]

-- | Define a function or value.
--
-- > f = x
-- > =====
-- > funBinds "f" [matchRhs [] "x"]
--
-- > id x = x
-- > =====
-- > funBinds "id" [matchRhs [var "x"] (var "x")]
--
-- > not True = False
-- > not False = True
-- > =====
-- > funBinds "not"
-- >   [ matchRhs [var "True"] (var "False")
-- >   , matchRhs [var "False"] (var "True")
-- >   ]
funBinds :: HasValBind t => RawRdrName -> [RawMatch] -> t
funBinds name matches = bindB $ withPlaceHolder
        (noExt FunBind name'
            (matchGroup context matches) WpHole)
        []
  where
    name' = valueRdrName name
    context = FunRhs name' Prefix NoSrcStrict

-- | Define a function that has a single case.
--
-- > f = x
-- > =====
-- > funBind "f" (matchRhs [] "x")
--
-- > id x = x
-- > =====
-- > funBind "id" $ matchRhs [var "x"] (var "x")
--
funBind :: HasValBind t => RawRdrName -> RawMatch -> t
funBind name m = funBinds name [m]

{- $rawMatch

A function definition is made up of one or more 'RawMatch' terms.  Each
'RawMatch' corresponds to a single pattern match.  For example, to define the
"not" function:

> not True = False
> not False = True

We could using a list of two 'RawMatch'es:

> funBinds "not"
>   [ matchRhs [var "True"] (var "False")
>   , matchRhs [var "False"] (var "True")
>   ]

A match may consist of one or more guarded expressions.  For example, to
define the function as:

> not x
>   | x = False
>   | otherwise = True

We would say:

> funBinds "not"
>   [ var "x" ==> match
>       [ guardedStmt (var "x") (rhs (var "False"))
>       , guardedStmt (var "otherwise") (rhs (var "True"))
>       ]
>   ]
-}

-- | Construct a function match consisting of multiple guards.
match :: [Pat'] -> [RawGRHS] -> RawMatch
match ps grhss = RawMatch ps grhss mempty

-- | Construct a function match with a single case.
matchRhs :: [Pat'] -> HsExpr' -> RawMatch
matchRhs ps e = match ps [rhs e]

where' :: RawMatch -> [RawValBind] -> RawMatch
where' r vbs = r { rawWhere = rawWhere r ++ vbs }

rhs :: HsExpr' -> RawGRHS
rhs = RawGRHS []

guarded :: [Stmt'] -> RawGRHS -> RawGRHS
guarded ss (RawGRHS ss' e) = RawGRHS (ss ++ ss') e

guardedStmt :: HsExpr' -> RawGRHS -> RawGRHS
guardedStmt e = guarded [stmt e]

-- | An expression statement.  May be used in a do expression (with 'do'') or in a
-- match (with 'guarded').
--
-- TODO: also allow using statements in list comprehensions.
stmt :: HsExpr' -> Stmt'
-- For now, don't worry about rebindable syntax.
stmt e =
    withPlaceHolder $ noExt BodyStmt (builtLoc e) noSyntaxExpr noSyntaxExpr

-- | A statement that binds a pattern.
--
-- > x <- act
-- > =====
-- > var "x" <-- var "act"
(<--) :: Pat' -> HsExpr' -> Stmt'
p <-- e = withPlaceHolder $ noExt BindStmt (builtPat p) (builtLoc e) noSyntaxExpr noSyntaxExpr
infixl 1 <--

-- | Syntax types which can declare/define functions.  For example:
-- declarations, or the body of a class declaration or class instance.
--
-- Use 'typeSig' or 'typeSigs' to declare that functions or values have
-- types, and use 'funBind' to give them definitions.
class HasValBind t where
    sigB :: Sig' -> t
    bindB :: HsBind' -> t

instance HasValBind RawValBind where
    sigB = SigV
    bindB = BindV

instance HasValBind HsDecl' where
    sigB = noExt SigD
    bindB = noExt ValD
