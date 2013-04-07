\section{Scanner and Parser for context-free grammars}

In this section we provide code for a simple scanner and parser for a textual representation of a context free grammar.

The grammar for the concrete representation follows the suggestion in the assignment, with one minor difference:

\begin{verbatim}

Grammar -> Grammar Production
Grammar -> Production
Production -> UpperSymbol Arrow RHS
RHS -> RHS Symbol
RHS ->
Symbol -> UpperSymbol
Symbol -> LowerSymbol

\end{verbatim}

In other words, non-terminals are restricted to being upper case (in our case, only the first letter needs to be capitalized), terminals are lower case (the first letter), neither can begin with a numeral, and right hand side terminals and non-terminals are delimited by spaces. 

\begin{code}

module ScanAndParse where

import ContextFreeGrammar
import Data.Char (isUpper,isSpace, isAlphaNum, isAlpha, isDigit)

data GrammarToken = 
  Symbol String |
  ArrowToken |
  NewLineToken deriving (Show, Eq)

alphanumeric = takeWhile isAlphaNum

drop' _  [] = []
drop' i (x:xs) = 
  if i <= 0 then (x:xs)
  else
    drop' (i-1) xs

\end{code}

The scanner is a simple function that checks for two special characters, the arrow, {\tt ->} and the newline character, {\tt \\n}, scans symbols for nonterminals or terminals, and returns their appropriate tokens.

If a non alphanumeric character is found, the scanner returns an error.

\begin{code}

scan :: String -> [GrammarToken]
scan []                    = []
scan ('-':'>':cs)          = ArrowToken:scan cs
scan ('\n':cs)             = NewLineToken:scan cs
scan (c:cs) | isSpace c    = scan cs
scan s@(c:cs) | isAlpha c  = 
  let name = alphanumeric s
      len  = length name in
        (Symbol name):scan (drop' len s)
scan s@(c:cs) | isDigit c  = 
  error "lexical error; symbols cannot begin with numerals."
scan s@(c:cs)              = 
  error ("lexical error; " ++ c:" is an unrecognized character.")

\end{code}

The parser generates a list of productions, i.e., a ``grammar'', from a list of grammar tokens. The helper function, |parseRHS|, will throw an syntax error if an arrow token is found on the right hand side.

The function |parse| will throw an error if multiple non-terminals occur on the left-hand side, or an arrow is missing.

\begin{code}

parseRHS :: [GrammarToken] -> ((RHS String String), [GrammarToken])
parseRHS [] =
   (Empty,[])
parseRHS (NewLineToken:rhs) =
   (Empty,rhs)
parseRHS (ArrowToken:rhs) =
  error "syntax error; arrow token found on right hand side"
parseRHS ((Symbol (c:cs)):rhs) =
  let (term,rhs') = parseRHS rhs in
  if isUpper c then
     ((NonT (c:cs) term), rhs')
  else 
     ((Term (c:cs) term), rhs')

parse :: [GrammarToken] -> Grammar String String
parse [] = []
parse (NewLineToken:[]) = []
parse ((Symbol s):ArrowToken:rhs) = 
  let (production,rhs') = parseRHS rhs in
  (Production s (production)):parse rhs'
parse ((Symbol s):rhs) = 
  error "Missing arrow or multiple non-terminals on left-hand side."

\end{code}