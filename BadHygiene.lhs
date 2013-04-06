\begin{code}
module BadHygiene where

import ContextFreeGrammar
import qualified Data.Set as S
import Control.Monad.State

reachable :: Grammar nt t -> Grammar nt t
reachable g = undefined where
  unreachable = sniff g


sniff :: Grammar nt t -> S.Set nt
sniff ps = execState (mapM computeUnreachable ps) S.empty where
  computeUnreachable :: Production nt t -> State (S.Set nt) ()
  computeUnreachable (Production nt rhs) = do
    set <- get
    when (S.member nt set) 
      addAll rhs
  addAll :: RHS nt t -> State (S.Set nt) ()    
  addAll (NonT nt rhs) = do
    set <- get
    put (S.insert nt set)
    addAll rhs
  addAll (Term t rhs) = addAll rhs
  addAll Empty = return ()
  
\end{code}