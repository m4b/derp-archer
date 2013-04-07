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
  cleanProductions = Filterable.filter (`S.member` reachable) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt reachable) cleanProductions

computeGenerating :: Ord nt => Grammar nt t -> S.Set nt
computeGenerating g = undefined

eliminateNonGenerating :: Ord nt => Grammar nt t -> Grammar nt t
eliminateNonGenerating g = undefined

eliminateUseless :: Ord nt => Grammar nt t -> Grammar nt t
eliminateUseless = eliminateUnreachable . eliminateNonGenerating

isEmptyGrammar :: (Eq t, Ord nt) => Grammar nt t -> Bool
isEmptyGrammar [] = True
isEmptyGrammar g = not . elem s $ g' where
  g' = eliminateNonGenerating g
  s = head g   
      
extractNonT (NonT nt rhs) = nt : extractNonT rhs
extractNonT (Term _ rhs) = extractNonT rhs
extractNonT Empty = []
  
simpleGrammar :: Grammar String String
simpleGrammar = [a,a',b,c,d,e] where
  a = Production "A" (Term "a" Empty)
  a' = Production "A" (NonT "B" Empty)
  b = Production "B" (NonT "B" (NonT "C" Empty))
  c = Production "C" (Term "a" (NonT "D" Empty))
  d = Production "D" (NonT "B" (Term "b" Empty))
  e = Production "F" Empty
  
\end{code}