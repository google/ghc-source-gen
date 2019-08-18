-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module provides combinators for constructing Haskell declarations.
module GHC.SourceGen.Binds
    (  -- * Bindings
      HsBind'
    , HasValBind
      -- * Type signatures
    , typeSig
    , typeSigs
      -- * Functions
    , funBind
    , funBinds
      -- * Values
    , valBindRhs
    , valBind
    -- ** Patterns
    , HasPatBind
    , patBindRhs
    , patBind
    -- * Matches
    -- $rawMatch
    , RawMatch
    , match
    , matchRhs
    -- * Right-hand sides
    , RawGRHSs
    , rhs
    -- ** Guards
    , guardedRhs
    , GuardedExpr
    , GRHS'
    , guards
    , guard
    -- ** Where clauses
    , where'
    , RawValBind
    -- * Statements
    , stmt
    , (<--)
    ) where

import BasicTypes (LexicalFixity(..))
import HsBinds
import HsExpr
import HsTypes
import TcEvidence (HsWrapper(WpHole))

import GHC.SourceGen.Binds.Internal
import GHC.SourceGen.Name
import GHC.SourceGen.Name.Internal
import GHC.SourceGen.Syntax.Internal
import GHC.SourceGen.Type.Internal (sigWcType)

-- | Declares the type of multiple functions or values.
--
-- > f, g :: A
-- > =====
-- > typeSigs ["f", "g"] (var "A")
typeSigs :: HasValBind t => [OccNameStr] -> HsType' -> t
typeSigs names t =
    sigB $ noExt TypeSig (map (typeRdrName . unqual) names)
        $ sigWcType t

-- | Declares the type of a single function or value.
--
-- > f :: A
-- > =====
-- > typeSig "f" (var "A")
typeSig :: HasValBind t => OccNameStr -> HsType' -> t
typeSig n = typeSigs [n]

-- | Defines a function or value.
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
-- >   [ matchRhs [conP "True" []] (var "False")
-- >   , matchRhs [conP "False" []] (var "True")
-- >   ]
funBinds :: HasValBind t => OccNameStr -> [RawMatch] -> t
funBinds name matches = bindB $ withPlaceHolder
        (noExt FunBind name'
            (matchGroup context matches) WpHole)
        []
  where
    name' = valueRdrName $ unqual name
    context = FunRhs name' Prefix NoSrcStrict

-- | Defines a function that has a single case.
--
-- > f = x
-- > =====
-- > funBind "f" (matchRhs [] "x")
--
-- > id x = x
-- > =====
-- > funBind "id" $ matchRhs [var "x"] (var "x")
--
funBind :: HasValBind t => OccNameStr -> RawMatch -> t
funBind name m = funBinds name [m]

-- | Defines a value consisting of multiple guards.
--
-- The resulting syntax is the same as a function with no arguments.
--
-- > x = y
-- > =====
-- > valBind "x" $ rhs $ var "y"
--
-- > x
-- >   | test = 1
-- >   | otherwise = 2
-- > =====
-- > valBind "x"
-- >   $ guardedRhs
-- >     [ var "test" `guard` int 1
-- >     , var "otherwise" `guard` int 2
-- >     ]
valBind :: HasValBind t => OccNameStr -> RawGRHSs -> t
valBind name = funBind name . match []

-- | Defines a value without any guards.
--
-- The resulting syntax is the same as a function with no arguments.
--
-- > x = y
-- > =====
-- > valBindRhs "x" $ var "y"
valBindRhs :: HasValBind t => OccNameStr -> HsExpr' -> t
valBindRhs name = valBind name . rhs

-- | Defines a pattern binding consisting of multiple guards.
--
-- > (x, y) = e
-- > =====
-- > patBind (tuple [var "x", var "y"]) $ rhs e
--
-- > (x, y)
-- >   | test = (1, 2)
-- >   | otherwise = (2, 3)
-- > =====
-- > patBind (tuple [var "x", var "y"])
-- >   $ guardedRhs
-- >       [ var "test" `guard` tuple [int 1, int 2]
-- >       , var "otherwise" `guard` [int 2, int 3]
-- >       ]
patBind :: HasPatBind t => Pat' -> RawGRHSs -> t
patBind p g =
    bindB
        $ withPlaceHolder
            (withPlaceHolder
                (noExt PatBind (builtPat p) (mkGRHSs g)))
        $ ([],[])

-- | Defines a pattern binding without any guards.
--
-- > (x, y) = e
-- > =====
-- > patBindRhs (tuple [var "x", var "y"]) e
patBindRhs :: HasPatBind t => Pat' -> HsExpr' -> t
patBindRhs p = patBind p . rhs

{- $rawMatch

A function definition is made up of one or more 'RawMatch' terms.  Each
'RawMatch' corresponds to a single pattern match.  For example, to define the
"not" function:

> not True = False
> not False = True

We could using a list of two 'RawMatch'es:

> funBinds "not"
>   [ matchRhs [conP "True" []] (var "False")
>   , matchRhs [conP "False" [] (var "True")
>   ]

A match may consist of one or more guarded expressions.  For example, to
define the function as:

> not x
>   | x = False
>   | otherwise = True

We would say:

> funBind "not"
>      $ match [var "x"] $ guardedRhs
>          [ guard (var "x") (var "False")
>          , guard (var "otherwise") (var "True")
>          ]
-}

-- | A function match consisting of multiple guards.
match :: [Pat'] -> RawGRHSs -> RawMatch
match = RawMatch

-- | A function match with a single case.
matchRhs :: [Pat'] -> HsExpr' -> RawMatch
matchRhs ps = match ps . rhs

-- | Adds a "where" clause to an existing 'RawGRHSs'.
--
-- > f x = y
-- >   where y = x
-- > =====
-- > funBind "x"
-- >   $ match [var "x"]
-- >   $ rhs (var "y")
-- >      `where` [valueRhs (var "y") $ var "x']
where' :: RawGRHSs -> [RawValBind] -> RawGRHSs
where' r vbs = r { rawGRHSWhere = rawGRHSWhere r ++ vbs }

-- | A right-hand side of a match, with no guards.
rhs :: HsExpr' -> RawGRHSs
rhs e = guardedRhs [guards [] e]

-- | A guarded right-hand side of a match.
--
-- >   | x = False
-- >   | otherwise = True
-- > =====
-- > guardedRhs
-- >   [ guard (var "x") (var "False")
-- >   , guard (var "otherwise") (var "True")
-- >   ]
guardedRhs :: [GuardedExpr] -> RawGRHSs
guardedRhs ss = RawGRHSs ss []

-- | An expression guarded by a single boolean statement.
--
-- >   | otherwise = ()
-- > =====
-- > guard (var "otherwise") unit
guard :: HsExpr' -> HsExpr' -> GuardedExpr
guard s = guards [stmt s]

-- | An expression guarded by multiple statements, using the @PatternGuards@ extension.
--
-- >   | Just y <- x, y = ()
-- > =====
-- > guards [conP "Just" (var "x") <-- var "y", var "x"] unit
guards :: [Stmt'] -> HsExpr' -> GuardedExpr
guards stmts e = noExt GRHS (map builtLoc stmts) (builtLoc e)

-- | An expression statement.  May be used in a do expression (with 'do'') or in a
-- match (with 'guard').
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

-- | Syntax types which can declare/define pattern bindings.
-- For example: declarations at the top-level or in let/where clauses.
--
-- Note: this class is more restrictive than 'HasValBind' since pattern
-- bindings cannot be used in class or instance declarations.
class HasValBind t => HasPatBind t where

instance HasPatBind RawValBind where
instance HasPatBind HsDecl' where
