\section{Scanner and Parser for context-free grammars}

In this section we provide code for scanning and parsing a textual representation of a context free grammar.

The concrete representation is as follows:

\begin{verbatim}

STUFF

\end{verbatim}

We used the Parselib module to ease scanning and parsing from a textual representation.

\begin{code}

module ScannerAndParser where

import ContextFreeGrammar
import Parselib
import Data.Char (isUpper,isLower,isSpace)

toTerms :: [Char] -> RHS Char Char
toTerms [] = Empty
toTerms (x:xs) =
  Term x (toTerms xs)

toNonTerms :: [Char] -> RHS Char Char
toNonTerms [] = Empty
toNonTerms (x:xs) =
  NonT x (toNonTerms xs)
  

--toRHS :: [[Char]] -> RHS String String
toRHS [] = Empty
toRHS (nt:nts) | isUpper . head $ nt =
  NonT nt (toRHS nts)
toRHS (t:ts) | isLower . head $ t =
  Term t (toRHS ts)
toRHS (x:xs) = error 
      (x ++ " is not a supported terminal nor non-terminal symbol. Aborting.")
           
--contents <- readFile "tests/test1.txt"

foo0 = "B -> \nB -> C\nC -> B\n"

parseEmpty = do
  many (char ' ')-- +++   many (char '\t')
  char '\n'
  return Empty

parseSymbol = do
  symb <- many alphanum
  space
  return symb

rhsParser = 
-- do {many (sat isSpace); return Empty} +++ 
 parseEmpty +++ 
 do
  {space;
--  list <- many alphanum;
  list <- many parseSymbol;
  return . toRHS $ list}
  
productionParser :: Parser (Production [Char] [Char])
productionParser = do
  space
  nt <- upper
  nts <- many alphanum
  space
  string "->"
  rhs <- rhsParser
  return $ (Production (nt:nts) Empty)
                  
grammarParser = do
  grammar <- many productionParser
  return grammar



\end{code}