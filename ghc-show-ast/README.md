ghc-show-ast helps debug the behavior of GHC that ghc-source-gen is trying to imitate.
This program parses a source file with GHC and then pretty-prints the AST.

To use:

    stack run ghc-show-ast -- path/to/file.hs
