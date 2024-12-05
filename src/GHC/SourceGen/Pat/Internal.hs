{-# LANGUAGE CPP #-}
module GHC.SourceGen.Pat.Internal where

import GHC.Hs.Pat (Pat(..))
#if MIN_VERSION_ghc(9,10,0)
import GHC.Parser.Annotation (EpToken(..), noSpanAnchor)
#endif
import GHC.Hs.Type (HsConDetails(..))
import GHC.Types.SrcLoc (unLoc)

import GHC.SourceGen.Lit.Internal (litNeedsParen, overLitNeedsParen)
import GHC.SourceGen.Syntax.Internal

-- Note: GHC>=8.6 inserts parentheses automatically when pretty-printing patterns.
-- When we stop supporting lower versions, we may be able to simplify this.
parenthesize :: Pat' -> Pat'
parenthesize p
    | needsPar p = parPat p
    | otherwise = p


needsPar :: Pat' -> Bool
needsPar (LitPat _ l) = litNeedsParen l
needsPar (NPat _ l _ _) = overLitNeedsParen $ unLoc l
#if MIN_VERSION_ghc(9,2,0)
needsPar (ConPat _ _ (PrefixCon _ xs)) = not $ null xs
#else
needsPar (ConPat _ _ (PrefixCon xs)) = not $ null xs
#endif
needsPar (ConPat _ _ (InfixCon _ _)) = True
needsPar SigPat{} = True
needsPar _ = False

parPat :: Pat' -> Pat'
#if MIN_VERSION_ghc(9,10,0)
parPat p = ParPat (EpTok noSpanAnchor, EpTok noSpanAnchor) (builtPat p)
#elif MIN_VERSION_ghc(9,4,0)
parPat p = withEpAnnNotUsed ParPat mkToken (builtPat p) mkToken
#else
parPat = withEpAnnNotUsed ParPat . builtPat
#endif

