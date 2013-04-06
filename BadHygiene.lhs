\begin{code}
module BadHygiene where

import ContextFreeGrammar
import qualified Data.Set as S
import Control.Monad.State

reachable :: Ord nt => Grammar nt t -> Grammar nt t
reachable g = undefined where
  canReach = sniff g


sniff :: Ord nt => Grammar nt t -> S.Set nt
sniff ps = snd . execState (while notDone (addNTs ps ps)) $ (S.empty,S.singleton . nonterminal . head $ ps) where
  notDone :: Ord nt => State (S.Set nt,S.Set nt) Bool
  notDone = fmap (not . uncurry (==)) get
  
  addNTs :: Ord nt => [Production nt t] -> [Production nt t] -> State (S.Set nt, S.Set nt) ()
  addNTs [] orig = addNTs orig orig
  addNTs ((Production nt rhs):ps) orig = do
    (_,s1) <- get
    when (S.member nt s1) (addAllNonT rhs)
    
  addAllNonT :: Ord nt => RHS nt t -> State (S.Set nt,S.Set nt) ()    
  addAllNonT (NonT nt rhs) = do
    (s0,s1) <- get
    unless (S.member nt s1) (put (s1,S.insert nt s1))
    addAllNonT rhs
  addAllNonT (Term t rhs) = addAllNonT rhs
  addAllNonT Empty = return ()
  


while :: Monad m => m Bool -> m () -> m ()  
while check action = do
  b <- check
  when b (action >> while check action)
  
simpleGrammar :: Grammar String String
simpleGrammar = [a,b,c,d,e] where
  a = Production "A" (Term "a" Empty)
  b = Production "B" (NonT "B" Empty)
  c = Production "C" (Term "a" (NonT "B" Empty))
  d = Production "D" (NonT "B" (Term "a" Empty))
  e = Production "F" Empty
  
\end{code}