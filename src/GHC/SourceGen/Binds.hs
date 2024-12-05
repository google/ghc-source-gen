-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE CPP #-}
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
    , funBindsWithFixity
      -- * Values
    , valBind
    , valBindGRHSs
    -- ** Patterns
    , HasPatBind
    , patBind
    , patBindGRHSs
    -- * Matches
    -- $rawMatch
    , RawMatch
    , match
    , matchGRHSs
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

import GHC (LexicalFixity(..))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Type
import GHC.Plugins (isSymOcc)

#if MIN_VERSION_ghc(9,10,0)
import GHC.Parser.Annotation (noAnn)
#endif
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
#if MIN_VERSION_ghc(9,10,0)
    sigB $ TypeSig ann (map (typeRdrName . unqual) names)
        $ sigWcType t
  where
    ann = AnnSig noAnn []
#else
    sigB $ withEpAnnNotUsed TypeSig (map (typeRdrName . unqual) names)
        $ sigWcType t
#endif

-- | Declares the type of a single function or value.
--
-- > f :: A
-- > =====
-- > typeSig "f" (var "A")
typeSig :: HasValBind t => OccNameStr -> HsType' -> t
typeSig n = typeSigs [n]

-- | Defines a function or value, with an explicit fixity. When given
-- 'Nothing', use infix notation iff the given name is symbolic.
--
-- > id x = x
-- > =====
-- > funBindsWithFixity (Just Prefix) "id" [match [var "x"] (var "x")]
--
-- > True && True = True
-- > True && False = False
-- > =====
-- > funBindsWithFixity Nothing "not"
-- >   [ match [conP "True" []] (var "False")
-- >   , match [conP "False" []] (var "True")
-- >   ]
funBindsWithFixity :: HasValBind t => Maybe LexicalFixity -> OccNameStr -> [RawMatch] -> t
funBindsWithFixity fixity name matches = bindB $ withPlaceHolder
        (noExt FunBind name'
            (matchGroup context matches)
            )
#if !MIN_VERSION_ghc(9,6,0)
        []
#endif
  where
    name' = valueRdrName $ unqual name
    occ = valueOccName name
    fixity' = fromMaybe (bool Prefix Infix $ isSymOcc occ) fixity
    context = FunRhs name' fixity' NoSrcStrict

-- | Defines a function or value.
--
-- > f = x
-- > =====
-- > funBinds "f" [match [] "x"]
--
-- > id x = x
-- > =====
-- > funBinds "id" [match [var "x"] (var "x")]
--
-- > not True = False
-- > not False = True
-- > =====
-- > funBinds "not"
-- >   [ match [conP "True" []] (var "False")
-- >   , match [conP "False" []] (var "True")
-- >   ]
funBinds :: HasValBind t => OccNameStr -> [RawMatch] -> t
funBinds = funBindsWithFixity (Just Prefix)

-- | Defines a function that has a single case.
--
-- > f = x
-- > =====
-- > funBind "f" (match [] "x")
--
-- > id x = x
-- > =====
-- > funBind "id" $ match [bvar "x"] (var "x")
--
funBind :: HasValBind t => OccNameStr -> RawMatch -> t
funBind name m = funBinds name [m]

-- | Defines a value consisting of multiple guards.
--
-- The resulting syntax is the same as a function with no arguments.
--
-- > x
-- >   | test = 1
-- >   | otherwise = 2
-- > =====
-- > valBindGRHSs "x"
-- >   $ guardedRhs
-- >       [ var "test" `guard` int 1
-- >       , var "otherwise" `guard` int 2
-- >       ]
valBindGRHSs :: HasValBind t => OccNameStr -> RawGRHSs -> t
valBindGRHSs name = funBind name . matchGRHSs []

-- | Defines a value without any guards.
--
-- The resulting syntax is the same as a function with no arguments.
--
-- > x = y
-- > =====
-- > valBind "x" $ var "y"
valBind :: HasValBind t => OccNameStr -> HsExpr' -> t
valBind name = valBindGRHSs name . rhs

-- | Defines a pattern binding consisting of multiple guards.
--
-- > (x, y)
-- >   | test = (1, 2)
-- >   | otherwise = (2, 3)
-- > =====
-- > patBindGrhs (tuple [bvar "x", bvar "y"])
-- >   $ guardedRhs
-- >       [ var "test" `guard` tuple [int 1, int 2]
-- >       , var "otherwise" `guard` [int 2, int 3]
-- >       ]
patBindGRHSs :: HasPatBind t => Pat' -> RawGRHSs -> t
patBindGRHSs p g =
#if MIN_VERSION_ghc(9,10,0)
    bindB
        $ withPlaceHolder
            (withPlaceHolder
                (noExt PatBind (builtPat p) (noExt HsNoMultAnn) (mkGRHSs g)))
#elif MIN_VERSION_ghc(9,6,0)
    bindB
        $ withPlaceHolder
            (withPlaceHolder
                (withEpAnnNotUsed PatBind (builtPat p) (mkGRHSs g)))
#else
    bindB
        $ withPlaceHolder
            (withPlaceHolder
                (withEpAnnNotUsed PatBind (builtPat p) (mkGRHSs g)))
        $ ([],[])
#endif

-- | Defines a pattern binding without any guards.
--
-- > (x, y) = e
-- > =====
-- > patBind (tuple [bvar "x", bvar "y"]) e
patBind :: HasPatBind t => Pat' -> HsExpr' -> t
patBind p = patBindGRHSs p . rhs

{- $rawMatch

A function definition is made up of one or more 'RawMatch' terms.  Each
'RawMatch' corresponds to a single pattern match.  For example, to define the
"not" function:

> not True = False
> not False = True

We could using a list of two 'RawMatch'es:

> funBinds "not"
>   [ match [conP "True" []] (var "False")
>   , match [conP "False" [] (var "True")
>   ]

A match may consist of one or more guarded expressions.  For example, to
define the function as:

> not x
>   | x = False
>   | otherwise = True

We would say:

> funBind "not"
>      $ matchGRHSs [bvar "x"] $ guardedRhs
>          [ guard (var "x") (var "False")
>          , guard (var "otherwise") (var "True")
>          ]
-}

-- | A function match consisting of multiple guards.
matchGRHSs :: [Pat'] -> RawGRHSs -> RawMatch
matchGRHSs = RawMatch

-- | A function match with a single case.
match :: [Pat'] -> HsExpr' -> RawMatch
match ps = matchGRHSs ps . rhs

-- | Adds a "where" clause to an existing 'RawGRHSs'.
--
-- > f x = y
-- >   where y = x
-- > =====
-- > funBind "x"
-- >   $ matchGRHSs [bvar "x"]
-- >   $ rhs (var "y")
-- >      `where` [valBind "y" $ var "x']
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
-- > guards [conP "Just" (bvar "x") <-- var "y", bvar "x"] unit
guards :: [Stmt'] -> HsExpr' -> GuardedExpr
#if MIN_VERSION_ghc(9,10,0)
guards stmts e = GRHS noAnn (map mkLocated stmts) (mkLocated e)
#else
guards stmts e = withEpAnnNotUsed GRHS (map mkLocated stmts) (mkLocated e)
#endif

-- | An expression statement.  May be used in a do expression (with 'do'') or in a
-- match (with 'guard').
--
-- TODO: also allow using statements in list comprehensions.
stmt :: HsExpr' -> Stmt'
-- For now, don't worry about rebindable syntax.
stmt e =
    withPlaceHolder $ noExt BodyStmt (mkLocated e) noSyntaxExpr noSyntaxExpr

-- | A statement that binds a pattern.
--
-- > x <- act
-- > =====
-- > bvar "x" <-- var "act"
(<--) :: Pat' -> HsExpr' -> Stmt'
#if MIN_VERSION_ghc(9,10,0)
p <-- e = withPlaceHolder $ BindStmt [] (builtPat p) (mkLocated e)
#else
p <-- e = withPlaceHolder $ withEpAnnNotUsed BindStmt (builtPat p) (mkLocated e)
#endif
infixl 1 <--

-- | Syntax types which can declare/define pattern bindings.
-- For example: declarations at the top-level or in let/where clauses.
--
-- Note: this class is more restrictive than 'HasValBind' since pattern
-- bindings cannot be used in class or instance declarations.
class HasValBind t => HasPatBind t where

instance HasPatBind RawValBind where
instance HasPatBind HsDecl' where
