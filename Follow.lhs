\begin{code}
module Follow where

import ContextFreeGrammar
import Control.Monad.State
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import First
import Nullable
import ScanAndParse


data GrammarState nt t = GS {
  grammar :: Grammar nt t,
  follows :: M.Map nt (S.Set (Terminal t)),
  firsts :: M.Map nt (S.Set (Terminal t))
}

follow :: (Ord nt, Ord t) 
          => Grammar nt t 
          -> M.Map nt (S.Set (Terminal t))
follow [] = M.empty
follow g@((Production s rhs):ps) = fs where
  state = mapM follow' . concat . replicate (length g) $ g
  fs = follows . execState state . GS g initial . first $ g
  initial = M.singleton s (S.singleton EOF)
  
follow' (Production a rhs) = do
  fllostate <- get
  let g = grammar fllostate
      fs = firsts fllostate
      fllow = follows fllostate
      ps = getProductionsWith a g
  sets <- forM ps $ \(Production x rhs) -> do
    case after a rhs of
      Empty -> return . fromMaybe S.empty . M.lookup x $ fllow
      Term t _ -> return . S.singleton . Terminal $ t
      NonT nt rest -> do
        let frstb = fs M.! nt
            fllowx = case S.member Epsilon frstb of
              False -> S.empty
              True -> fromMaybe S.empty . M.lookup x $ fllow
        return $ S.union (S.delete Epsilon frstb) (fllowx)
  let s = fromMaybe S.empty (M.lookup a fllow)
      newS = S.union s (S.unions sets)
  put fllostate{follows = M.insert a newS fllow}
        
  

after :: (Eq nt) => nt -> RHS nt t -> RHS nt t
after nt Empty = Empty
after nt (Term t rhs) = after nt rhs
after nt (NonT nt2 rhs) = if nt == nt2 
                          then rhs else after nt rhs 
  
getProductionsWith :: (Ord nt, Ord t) 
                      => nt 
                      -> Grammar nt t 
                      -> [Production nt t]
getProductionsWith nt ps = filter (elem nt . nonTerminals . rhs) ps

{--
follow :: (Ord nt, Ord t) => Grammar nt t -> M.Map nt (S.Set (Terminal t))
follow [] = M.empty
follow g@((Production nt rhs):ps) = M.adjust (S.insert EOF) nt fMap where
  fMap = M.fromList $ zip (map nonterminal g) sets
  sets = evalState (mapM (follow'' . nonterminal) g) (GS g (first g))

follow' :: (Ord nt, Ord t) => Grammar nt t -> Production nt t -> (nt,S.Set (Terminal t))
follow' g (Production a rhs) = (a,undefined) where
  xs = getProductionsWith a g
  firsts = first g

follow'' :: (Ord nt, Ord t) => nt -> State (GrammarState nt t) (S.Set (Terminal t))
follow'' a = do
  g <- gets grammar
  fs <- gets firsts
  let ps = getProductionsWith a g
  sets <- forM ps $ \(Production x (after a -> beta)) -> do
            case beta of
              Empty -> follow'' x
              NonT b _ -> do
                let firstb = fs M.! b
                case S.member Epsilon firstb of
                  True -> do
                    folb <- follow'' b
                    let fb2 = S.delete Epsilon firstb
                    return $ S.union folb fb2
                  False -> return firstb
              Term t _ -> return . S.singleton . Terminal $ t
  return . S.unions $ sets
    

getProductionsWith :: (Ord nt, Ord t) => nt -> Grammar nt t -> [Production nt t]
getProductionsWith nt ps = filter (elem nt . nonTerminals . rhs) ps


after :: (Eq nt) => nt -> RHS nt t -> RHS nt t
after nt Empty = Empty
after nt (Term t rhs) = after nt rhs
after nt (NonT nt2 rhs) = if nt == nt2 then rhs else after nt rhs

simpleGrammar :: Grammar String String
simpleGrammar = [s,s',b,a,c] where
  s = Production "S" (NonT "A" (NonT "B" Empty))
  s' = Production "S" (Term "x" Empty)
  b = Production "B" (Term "b" Empty)
  a = Production "A" (Term "a" (NonT "A" Empty))
  c = Production "C" (Term "d" Empty)
--}
\end{code}