\documentclass[11pt]{article}

%include polycode.fmt

%format `union` = "\cup"
%format alpha = "\alpha"
%format gamma = "\gamma"
%format delta = "\delta"
%format capGamma = "\Gamma"
%format tau = "\tau"
%format tau1 = "\tau_{1}"
%format tau2 = "\tau_{2}"
%format tau11 = "\tau_{11}"
%format tau12 = "\tau_{12}"
%format t12 = "t_{12}"
%format t1 = "t_{1}"
%format t1' = "t_{1}^{\prime}"
%format t2 = "t_{2}"
%format t2' = "t_{2}^{\prime}"
%format t3 = "t_{3}"
%format nv1 = "nv_{1}"

\author{\textsc{S. Patel, J. Collard, M. Barney}}
\title{Project 2: Grammar Analysis and Parsing}
\date{\today}

\begin{document}

\maketitle

\section{Introduction}

This report contains our implementation of a scanner and parser for context free grammars, a series of hygiene functions for sterilizing the grammar, and finally a parser \emph{for} the grammar specified in the context free grammar.

It is divided up into several sections, roughly corresponding to the problems given in the specification, each a Haskell module.  The work was split up evenly amongst the group members, and approximately 40 man hours went into the final preparation of this document, the source code, unit testing, and related work.

Our design decisions with respect to the final problems for strong-LL(1) and non-strong-LL(1) table driven parsers is given in \S\ref{parser}

%include ContextFreeGrammar.lhs
%include ScanAndParse.lhs
%include BadHygiene.lhs

\section{Nullable, First, and Follow}

In this section, we provide several modules for computing the nullable, first and follow sets of a given context free grammar, respectively.

%include Nullable.lhs
%include First.lhs
%include Follow.lhs

%include Table.lhs
%include Parser.lhs

\section{Main module}

The main module puts everything together, takes a textual representation of a context-free grammar as input, scans, parses, and performs the rest of the duties that are required.

Different tables are built depending on whether the function for building a strong LL(1) parse table returns |Nothing| or not.

\begin{code}

module Main where

import ContextFreeGrammar
import ScanAndParse
import BadHygiene
import Table
import Parser

import qualified Data.Map as M

import System.Environment

main = do 
     [file] <- getArgs
     contents <- readFile file
     let grammar = sparse contents
     let grammar' = eliminateUseless . getNewStart $ grammar
     let table = buildTable grammar'
     case table of 
          Nothing -> do
            generate grammar' (buildTableA grammar')
          Just t -> 
            generateStrong grammar' t

\end{code}

\section{Conclusion and Sample Output}

This concludes our implementation.  We have provided some sample output from the various stages of our implementation.  Two different simple context-free grammars are considered, {\tt 39.txt} and {\tt 39\_ambiguous.txt}.

\subsection{Strong LL(1) Parser for 39.txt}

\subsubsection{Grammar}

\begin{verbatim}
T -> R
T -> a T c
R -> 
R -> b R
\end{verbatim}


\subsubsection{Table}

\begin{verbatim}
(("R","$"),Production {nonterminal = "R", rhs = Empty})
(("R","b"),Production {nonterminal = "R", rhs = Term "b" (NonT "R" Empty)})
(("R","c"),Production {nonterminal = "R", rhs = Empty})
(("T","$"),Production {nonterminal = "T", rhs = NonT "R" Empty})
(("T","a"),Production {nonterminal = "T"
          , rhs = Term "a" (NonT "T" (Term "c" Empty))})
(("T","b"),Production {nonterminal = "T", rhs = NonT "R" Empty})
(("T","c"),Production {nonterminal = "T", rhs = NonT "R" Empty})
(("T'","$"),Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)})
(("T'","a"),Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)})
(("T'","b"),Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)})
\end{verbatim}

\subsubsection{Code}

\begin{verbatim}
module LLParser (parse) where 
import qualified Data.Map as M
import ContextFreeGrammar
import Data.Char

parseWithTable start t s = parse' [start] s where
  parse' [] cs = True
  parse' (top:rest) s@(c:cs)
    | isUpper (head top) = case M.lookup (top,[c]) t of
      Nothing -> False
      Just (Production _ rhs) -> parse' (rhsToList rhs ++ rest) s
    | c == head top = parse' rest cs
    | otherwise = False

rhsToList :: RHS String String -> [String]
rhsToList Empty = []
rhsToList (Term t rhs) = t : rhsToList rhs
rhsToList (NonT nt rhs) = nt : rhsToList rhs
parse s = parseWithTable "T'" table (s++"$")
\end{verbatim}

\subsection{LL(1) Parser for 39\_ambiguous.txt}

\subsubsection{Grammar}

\begin{verbatim}
T -> R
T -> a T c
R -> 
R -> R b R
\end{verbatim}

\subsubsection{Table}

\begin{verbatim}
(("R","$"),[Production {nonterminal = "R", rhs = Empty}])
(("R","b"),[Production {nonterminal = "R"
          , rhs = NonT "R" (Term "b" (NonT "R" Empty))}
          ,Production {nonterminal = "R", rhs = Empty}])
(("R","c"),[Production {nonterminal = "R", rhs = Empty}])
(("T","$"),[Production {nonterminal = "T", rhs = NonT "R" Empty}])
(("T","a"),[Production {nonterminal = "T"
          , rhs = Term "a" (NonT "T" (Term "c" Empty))}])
(("T","b"),[Production {nonterminal = "T", rhs = NonT "R" Empty}])
(("T","c"),[Production {nonterminal = "T", rhs = NonT "R" Empty}])
(("T'","$"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}])
(("T'","a"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}])
(("T'","b"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}])
\end{verbatim}

\subsubsection{Code}

\begin{verbatim}
module NonLLParser (parse) where 
import qualified Data.Map as M
import ContextFreeGrammar
import Data.Char

parseWithTableA start t s = parse' [start] s where
  parse' [] cs = True
  parse' _ []  = False
  parse' st@(top:rest) s@(c:cs)
    | isUpper (head top) = case M.lookup (top,[c]) t of
      Nothing -> False
      Just [Production _ rhs] -> parse' (rhsToList rhs ++ rest) s
      Just ps -> or . map (helper rest s) $ ps
    | c == head top = parse' rest cs
    | otherwise = False
  helper rest str (Production nt rhs) = 
    case nt == head newStack of
         False -> parse' newStack str
         True -> parse' (tail newStack) str
    where newStack = rhsToList rhs ++ rest

rhsToList :: RHS String String -> [String]
rhsToList Empty = []
rhsToList (Term t rhs) = t : rhsToList rhs
rhsToList (NonT nt rhs) = nt : rhsToList rhs
parse s = parseWithTableA "T'" tableA (s++"$")
\end{verbatim}


\end{document}