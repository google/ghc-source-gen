{-# LANGUAGE CPP #-}
module GHC.SourceGen.Pat.Internal where

import GHC.Hs.Pat (Pat(..))
import GHC.Hs.Types (HsConDetails(..))

import GHC.SourceGen.Lit.Internal (litNeedsParen, overLitNeedsParen)
import GHC.SourceGen.Syntax.Internal
import SrcLoc (unLoc)

-- Note: GHC>=8.6 inserts parentheses automatically when pretty-printing patterns.
-- When we stop supporting lower versions, we may be able to simplify this.
parenthesize :: Pat' -> Pat'
parenthesize p
    | needsPar p = parPat p
    | otherwise = p


needsPar :: Pat' -> Bool
#if MIN_VERSION_ghc(8,6,0)
needsPar (LitPat _ l) = litNeedsParen l
needsPar (NPat _ l _ _) = overLitNeedsParen $ unLoc l
#else
needsPar (LitPat l) = litNeedsParen l
needsPar (NPat l _ _ _) = overLitNeedsParen $ unLoc l
#endif
needsPar (ConPatIn _ (PrefixCon xs)) = not $ null xs
needsPar (ConPatIn _ (InfixCon _ _)) = True
needsPar ConPatOut{} = True
#if MIN_VERSION_ghc(8,6,0)
needsPar SigPat{} = True
#else
needsPar SigPatIn{} = True
needsPar SigPatOut{} = True
#endif
needsPar _ = False

parPat :: Pat' -> Pat'
parPat = noExt ParPat . builtPat

