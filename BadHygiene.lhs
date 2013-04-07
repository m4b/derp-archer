\begin{code}
module BadHygiene(computeReachable,
                  eliminateUnreachable,
                  computeGenerating,
                  eliminateNonGenerating,
                  eliminateUseless,
                  isEmptyGrammar) where

import ContextFreeGrammar
import qualified Data.Map as M
import qualified Data.Set as S
import Filterable
import Prelude hiding (drop)

computeReachable :: Ord nt => Grammar nt t -> S.Set nt
computeReachable [] = S.empty
computeReachable ps = go (S.singleton . nonterminal . head $ ps) (ps ++ ps) where
  go marked [] = marked
  go marked ((Production nt rhs):prs) = if S.member nt marked
                                        then go marked' prs
                                        else go marked prs
    where marked' = S.union marked . S.fromList . extractNonT $ rhs

eliminateUnreachable :: Ord nt => Grammar nt t -> Grammar nt t
eliminateUnreachable g = cleanGrammar where
  reachable = computeReachable $ g
  --unnecessary?  By definition, the unreachable non-terminals cannot be in any
  --other production list.
  --cleanProductions = Filterable.filter (`S.member` reachable) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt reachable) g

computeGenerating :: (Ord nt, Ord t) => Grammar nt t -> S.Set nt
computeGenerating [] = S.empty
computeGenerating ps = go S.empty (S.fromList . concatMap (extractT . rhs) $ ps) (ps ++ ps) where
  go :: (Ord nt, Ord t) => S.Set nt -> S.Set t -> Grammar nt t -> S.Set nt
  go markedNT _ [] = markedNT
  go markedNT markedT ((Production nt rhs):prs) = if (all (`S.member` markedT) . extractT $ rhs) &&
                                                     (all (`S.member` markedNT) . extractNonT $ rhs)
   then go (S.insert nt markedNT) markedT prs else go markedNT markedT prs

eliminateNonGenerating :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateNonGenerating g = cleanGrammar where
  generating = computeGenerating g
  cleanProductions = Filterable.filter (`S.member` generating) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt generating) cleanProductions

eliminateUseless :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateUseless = eliminateUnreachable . eliminateNonGenerating

isEmptyGrammar :: (Ord t, Ord nt) => Grammar nt t -> Bool
isEmptyGrammar [] = True
isEmptyGrammar g = not . elem nt . map nonterminal $ g' where
  g' = eliminateNonGenerating g
  (Production nt _) = head g   
      
extractNonT (NonT nt rhs) = nt : extractNonT rhs
extractNonT (Term _ rhs) = extractNonT rhs
extractNonT Empty = []

extractT (Term t rhs) = t : extractT rhs
extractT (NonT _ rhs) = extractT rhs
extractT Empty = []
  
simpleGrammar :: Grammar String String
simpleGrammar = [s,s',b,a,c] where
  s = Production "S" (NonT "A" (NonT "B" Empty))
  s' = Production "S" (Term "x" Empty)
  b = Production "B" (Term "b" Empty)
  a = Production "A" (Term "a" (NonT "A" Empty))
  c = Production "C" (Term "d" Empty)
  
\end{code}