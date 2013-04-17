\section{Generating a Parse Table}

In this section we generate a parse table for a given grammar, assuming it has been properly scanned, parsed, and thoroughly cleansed.

\begin{code}

module Table where

import ContextFreeGrammar
import qualified Data.Map as M
import Filterable
import Nullable
import First
import Follow
import System.Environment

import Data.List

import ScanAndParse
import BadHygiene

type Table nt t = M.Map nt (M.Map t (Production nt t))

foo1 :: Grammar String String
foo1 = [a,b,c,d] where
  a = Production "A" (Term "a" Empty)
  b = Production "B" (NonT "B" Empty)
  c = Production "C" (Term "a" (NonT "B" Empty))
  d = Production "D" (NonT "B" (Term "a" Empty))


getFeature feature productions = loop productions []
          where 
            loop [] acc = sort . nub . concat $ acc
            loop ((Production s rhs):xs) acc = loop xs ((feature rhs):acc)

buildTable grammar = 
           let terms = getFeature terminals grammar in
           let nterms = getFeature nonTerminals grammar in
           terms         


main = do
 contents <- readFile "tests/39.txt"
 let g' = eliminateUseless . sparse $ contents    
 let g'' = follow . sparse $ contents    
 putStrLn $ show (getFeature terminals g')
 putStrLn $ show g''
\end{code}