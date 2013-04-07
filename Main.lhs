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

%include ContextFreeGrammar.lhs
%include ScanAndParse.lhs

\section{Main module}

The main module puts everything together, takes an textual representation of a context-free grammar as input, scans, parses, and performs the rest of the duties that are required.

\begin{code}

module Main where

import ContextFreeGrammar
import ScanAndParse
import BadHygiene

import System.Environment

main = do 
     [file] <- getArgs
     contents <- readFile file
     putStrLn $ show contents

\end{code}


\end{document}