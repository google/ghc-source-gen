-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- | This module exports all of the definitions in this package in one
-- convenient location.
--
-- For more details and examples of usage, see
-- <https://github.com/google/ghc-source-gen>.
module GHC.SourceGen
    ( -- * Syntax types
      -- | These modules declare combinators for constructing different parts
      -- of a GHC syntax tree.
      module GHC.SourceGen.Syntax,
      module GHC.SourceGen.Name,
      module GHC.SourceGen.Decl,
      module GHC.SourceGen.Expr,
      module GHC.SourceGen.Module,
      module GHC.SourceGen.Pat,
      module GHC.SourceGen.Type,
      -- * Overloaded combinators
      -- | Certain concepts make sense in different
      -- parts of Haskell syntax.  For example, 'var' may be used in
      -- expressions, types, patterns, and import or export lists.
      module GHC.SourceGen.Binds,
      module GHC.SourceGen.Lit,
      module GHC.SourceGen.Overloaded,
      -- * Renders Haskell syntax into text
      module GHC.SourceGen.Pretty,
    ) where

import GHC.SourceGen.Binds
import GHC.SourceGen.Decl hiding (patBind)
import GHC.SourceGen.Expr
import GHC.SourceGen.Lit
import GHC.SourceGen.Module
import GHC.SourceGen.Name
import GHC.SourceGen.Overloaded
import GHC.SourceGen.Pat
import GHC.SourceGen.Pretty
import GHC.SourceGen.Syntax
import GHC.SourceGen.Type

