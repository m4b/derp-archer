\section{Hygiene Module}

In this module, we perform basic hygiene checks on the grammar, remove unreachable non terminals, etc.

\begin{code}
module BadHygiene(computeReachable,
                  eliminateUnreachable,
                  computeGenerating,
                  eliminateNonGenerating,
                  eliminateUseless,
                  isEmptyGrammar) where

import ContextFreeGrammar
import qualified Data.Set as S
import Filterable
import ScanAndParse


{-BEGIN CLEANING FUNCTIONS-}

\end{code}

  |computeReachable| finds the Set of all Non Terminals of a Grammar that
  can be reached from the start node.

\begin{code}

computeReachable :: Ord nt => Grammar nt t -> S.Set nt
computeReachable [] = S.empty
computeReachable ps = go (S.singleton . nonterminal . head $ ps) 
                 (concat . replicate (length ps) $ ps) where
  go marked [] = marked
  go marked ((Production nt rhs):prs) = if S.member nt marked
                                        then go marked' prs
                                        else go marked prs
    where marked' = S.union marked . S.fromList . nonTerminals $ rhs

\end{code}

  |eliminateUnreachable| removes all unreachable Non Terminals from
  a Grammar.

\begin{code}

eliminateUnreachable :: Ord nt => Grammar nt t -> Grammar nt t
eliminateUnreachable g = cleanGrammar where
  reachable = computeReachable $ g
  --unnecessary?  By definition, the unreachable non-terminals cannot be in any
  --other production list.
  --cleanProductions = Filterable.filter (`S.member` reachable) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt reachable) g

\end{code}

  |computeGenerating| finds the Set of all Non Terminals of a Grammar
  that can produce a string of Terminals.

\begin{code}
computeGenerating :: (Ord nt, Ord t) => Grammar nt t -> S.Set nt
computeGenerating [] = S.empty
computeGenerating ps = go S.empty (concat . replicate (length ps) $ ps) where
  allTerms = S.fromList . concatMap (terminals . rhs) $ ps
  go markedNT [] = markedNT
  go markedNT ((Production nt rhs):prs) = 
      if (all (`S.member` allTerms) . terminals $ rhs) &&
         (all (`S.member` markedNT) . nonTerminals $ rhs)
      then 
          go (S.insert nt markedNT) prs 
      else 
          go markedNT prs
\end{code}

  |eliminateNonGenerating| removes all non Generating Non Terminals from
  a Grammar.

\begin{code}

eliminateNonGenerating :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateNonGenerating g = cleanGrammar where
  generating = computeGenerating g
  cleanProductions = Filterable.filter (`S.member` generating) g
  cleanGrammar = Prelude.filter 
                 (\(Production nt rhs) -> S.member nt generating) cleanProductions

\end{code}

  |eliminateUseless| removes all non Generating and unreachable Non Terminals
  from a Grammar.

\begin{code}
eliminateUseless :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateUseless = eliminateUnreachable . eliminateNonGenerating

\end{code}

  |isEmptyGrammar| determines if a Grammar will produce any strings
  at all.

\begin{code}

isEmptyGrammar :: (Ord t, Ord nt) => Grammar nt t -> Bool
isEmptyGrammar [] = True
isEmptyGrammar g = not . elem nt . map nonterminal $ g' where
  g' = eliminateNonGenerating g
  (Production nt _) = head g   

{-END CLEANING FUNCTIONS-}
 

\end{code}