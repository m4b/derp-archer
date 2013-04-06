\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ContextFreeGrammar(Grammar, Production(..), RHS(..), module Dropable) where

import Dropable
import Prelude hiding(drop)

type Grammar nt t = [Production nt t]

instance (Eq nt) => Dropable nt (Grammar nt t) where
  drop x grammar = map (drop x) grammar

data Production nt t = Production {nonterminal :: nt,
                                   rhs         :: RHS nt t} deriving (Eq, Ord)

instance Show (Production String String) where
  show (Production nt rhs) = nt ++ " -> " ++ show rhs

instance (Eq nt) => Dropable nt (Production nt t) where
  drop x (Production nt rhs) = Production nt (drop x rhs)

data RHS nt t = Empty
              | Term t (RHS nt t)
              | NonT nt (RHS nt t) deriving (Eq, Ord)

instance Show (RHS String String) where
  show Empty = ""
  show (Term t rhs) = t ++ (show rhs)
  show (NonT nt rhs) = nt ++ (show rhs)

instance (Eq nt) => Dropable nt (RHS nt t) where
  drop x (NonT nt rhs) 
    | x == nt = drop x rhs
    | otherwise = (NonT nt (drop x rhs))
  drop x (Term t rhs) = Term t (drop x rhs)
  drop _ Empty = Empty

simpleGrammar :: Grammar String String
simpleGrammar = [a,b,c,d] where
  a = Production "A" (Term "a" Empty)
  b = Production "B" (NonT "B" Empty)
  c = Production "C" (Term "a" (NonT "B" Empty))
  d = Production "D" (NonT "B" (Term "a" Empty))

\end{code}

