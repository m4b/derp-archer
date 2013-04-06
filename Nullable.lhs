\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Nullable(nullable) where

import ContextFreeGrammar
import qualified Data.Set as S

import Prelude hiding(drop)

type Set = S.Set

nullable :: (Ord nt) => Grammar nt t -> Set nt
nullable = nullable' S.empty

nullable' :: (Ord nt) => Set nt -> Grammar nt t -> Set nt
nullable' set grammar = set'' where
  set'' = if nulls == set then set else set'
  set' = nullable' nulls (S.fold drop grammar nulls)
  nulls = S.fromList . map nonterminal . filter isNull $ grammar

isNull :: Production nt t -> Bool
isNull (Production _ Empty) = True
isNull _ = False

class Drop b a where
  drop :: b -> a -> a

instance (Eq nt) => Drop nt (RHS nt t) where
  drop x (NonT nt rhs) 
    | x == nt = drop x rhs
    | otherwise = (NonT nt (drop x rhs))
  drop x (Term t rhs) = Term t (drop x rhs)
  drop _ Empty = Empty

instance (Eq nt) => Drop nt (Production nt t) where
  drop x (Production nt rhs) = Production nt (drop x rhs)

instance (Eq nt) => Drop nt (Grammar nt t) where
  drop x grammar = map (drop x) grammar

simpleGrammar :: Grammar String String
simpleGrammar = [a] where
  a = Production "A" (Term "ab" Empty)

simpleGrammar2 :: Grammar String String
simpleGrammar2 = [a,a',b,b',c] where 
  a = Production "A" (Term "ab" Empty)
  a' = Production "A" Empty
  b = Production "B" (NonT "A" (NonT "A" Empty))
  b' = Production "B" (NonT "A" (Term "b" Empty))
  c = Production "C" (Term "cdef" Empty)

\end{code}