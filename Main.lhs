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

\section{Conclusion}

This concludes our implementation.  We provide some sample output of a generated 
binary run on a context free grammar specified in a text file, {\tt 39.txt}.


\end{document}