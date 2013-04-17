\section{Context Free Grammar}

In this section we provide the context free grammar data type.

At its heart, a grammar it consists of a list of productions, where each production consists of a constructor and two arguments; the first a paramaterized nonterminal, and the second a paramaterized right hand side.

An |RHS| is either empty, a terminal, which takes two arguments --- the paramaterized object representing a terminal, and another |RHS|; or a non-terminal, which similarly takes two arguments.

\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ContextFreeGrammar
 (Grammar, Production(..), RHS(..), module Dropable, 
 nonTerminals, terminals, Terminal(..)) where

import Dropable
import Filterable
import Prelude hiding(drop, filter)

type Grammar nt t = [Production nt t]

data Terminal t = Epsilon | EOF | Terminal t deriving (Show, Eq, Ord)

instance (Eq nt) => Dropable nt (Grammar nt t) where
  drop x grammar = map (drop x) grammar

instance Filterable (nt -> Bool) (Grammar nt t) where
  filter pred grammar = map (filter pred) grammar

data Production nt t = Production {nonterminal :: nt,
                                   rhs         :: RHS nt t} deriving (Eq, Ord)

instance Show (Production String String) where
  show (Production nt rhs) = nt ++ " ->" ++ show rhs

instance Show (Production Char Char) where
  show (Production nt rhs) = show nt ++ " -> " ++ show rhs

instance (Eq nt) => Dropable nt (Production nt t) where
  drop x (Production nt rhs) = Production nt (drop x rhs)

instance Filterable (nt -> Bool) (Production nt t) where
  filter pred (Production nt rhs) = Production nt (filter pred rhs)

data RHS nt t = Empty
              | Term t (RHS nt t)
              | NonT nt (RHS nt t) deriving (Eq, Ord)

instance Show (RHS String String) where
  show Empty = ""
  show (Term t rhs) = " " ++ t ++ (show rhs)
  show (NonT nt rhs) = " " ++ nt ++ (show rhs)

instance Show (RHS Char Char) where
  show Empty = ""
  show (Term t rhs) = show t ++ (show rhs)
  show (NonT nt rhs) = show nt ++ (show rhs)

instance (Eq nt) => Dropable nt (RHS nt t) where
  drop x (NonT nt rhs) 
    | x == nt = drop x rhs
    | otherwise = (NonT nt (drop x rhs))
  drop x (Term t rhs) = Term t (drop x rhs)
  drop _ Empty = Empty

instance Filterable (nt -> Bool) (RHS nt t) where
  filter _ Empty = Empty
  filter pred (Term t rhs) = (Term t (filter pred rhs))
  filter pred (NonT nt rhs) = 
         if pred nt then (NonT nt (filter pred rhs)) else (filter pred rhs)
  
\end{code}
  nonTerminals takes the RHS of a Production and
  returns a list of all Non Terminals
\begin{code}
nonTerminals :: RHS nt t -> [nt]  
nonTerminals (NonT nt rhs) = nt : nonTerminals rhs
nonTerminals (Term _ rhs) = nonTerminals rhs
nonTerminals Empty = []

\end{code}
  terminals takes the RHS of a Production and
  returns a list of all Terminals
\begin{code}
terminals :: RHS nt t -> [t]
terminals (Term t rhs) = t : terminals rhs
terminals (NonT _ rhs) = terminals rhs
terminals Empty = []

simpleGrammar :: Grammar String String
simpleGrammar = [a,b,c,d] where
  a = Production "A" (Term "a" Empty)
  b = Production "B" (NonT "B" Empty)
  c = Production "C" (Term "a" (NonT "B" Empty))
  d = Production "D" (NonT "B" (Term "a" Empty))

\end{code}

