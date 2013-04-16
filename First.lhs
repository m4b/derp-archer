\begin{code}
module First where

import ContextFreeGrammar
import qualified Data.Map as M
import qualified Data.Set as S
import Nullable

first :: (Ord nt, Ord t) => Grammar nt t -> M.Map nt (S.Set (Terminal t))
first = error "First is undefined"


first' _ (Production nt Empty) = (nt, S.singleton Epsilon)
first' _ (Production nt (Term t _)) = (nt, S.singleton . Terminal $ t)
first' g (Production nt ()

firstRHS _ Empty = S.singleton Epsilon
firstRHS _ (Term t _) = S.singleton . Terminal $ t
firstRHS g (NonT nt rhs) = where
  ps = filter ((== nt) . nonterminal) g

\end{code}