\section{Generating a Parse Table}

In this section we generate a parse table for a given grammar, assuming it has been properly scanned, parsed, and thoroughly cleansed.

\begin{code}

module Table where

import ContextFreeGrammar
import Filterable
import Nullable
import First
import Follow
import System.Environment

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

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

-- need to generate S' \(\to\) S\$
getNewStart [] = error "getNewStart run on empty grammar --- a new low point"
getNewStart (Production nt rhs:ps) = 
            Production (nt ++ "'") (NonT nt (Term "$" Empty))


isRHSNullable :: Ord a => S.Set a -> RHS a t -> Bool
isRHSNullable _ Empty = True
isRHSNullable _ (Term t _) = False
isRHSNullable m (NonT nt rhs) = 
              if S.member nt m then 
                 isRHSNullable m rhs
              else False

firstRHS _ Empty = S.empty
firstRHS i (Term t rhs) = S.insert (Terminal t) (firstRHS i rhs)
firstRHS i (NonT nt rhs) = 
         if (S.member nt (nulls i)) then 
            S.union firstsnt (firstRHS i rhs)
         else 
            firstsnt
  where firstsnt = ((firsts i) M.! nt)

data GrammarInfo nt t= GI {
  firsts :: M.Map nt (S.Set (Terminal t)),
  follows :: M.Map nt (S.Set (Terminal t)),
  nulls :: S.Set nt
} deriving Show


\end{code}

A production N \(\to \alpha \) is in the table (N,\emph a) \emph{iff} \emph a is in first(\(\alpha\)) \(\lor\) (nullable(\(\alpha\)) \(\land\) \emph a is in follow(\(\alpha\))).  This condition is readily translateable into our code:

\begin{code}



validEntry (Production _ alpha) term nterm gi = firstalpha || (nullablea && follown)
  where 
        firstalpha = S.member (term) (firstRHS gi alpha)
        nullablea = isRHSNullable (nulls gi) alpha
        follown = S.member (term) ((follows gi) M.! nterm)

toTerminal [] = []
toTerminal ("$":ss) = EOF:toTerminal ss
toTerminal (s:ss) = Terminal s:toTerminal ss

fromTerminal' Epsilon      = ""
fromTerminal' EOF          = "$"
fromTerminal' (Terminal t) = t
fromTerminalSet ts = map fromTerminal' $ S.toList ts

columns p t [] gi =  []
columns p t (nt:nts) gi = 
     if validEntry p t nt gi then
        (t,p):(columns p t (nts) gi)
     else
        columns p t nts gi

rows p [] nts gi = []
rows p (t:ts) nts gi = (columns p t nts gi):(rows p ts nts gi)

buildTable' [] terms nterms gi = []
buildTable' (p:ps) terms nterms gi = (concat (rows p terms nterms gi)):buildTable' ps terms nterms gi

buildTable grammar = 
           let grammar' = (getNewStart grammar) : grammar in
           let terms = toTerminal . getFeature terminals $ grammar' in
           let nterms = getFeature nonTerminals grammar' in
           let gi = GI (first grammar') (follow grammar') (nullable grammar') in
           let table = buildTable' grammar' terms nterms gi in
           concat table

man = do
 contents <- readFile "tests/39.txt"
 let g = sparse contents
 let g' = eliminateUseless . sparse $ contents    
 let gnull = nullable. sparse $ contents    
 let gfollow = follow . sparse $ contents    
 let gfirst = first . sparse $ contents
 let gi = GI gfirst gfollow gnull
 putStrLn $ show (getFeature terminals g')
 putStrLn $ show (getFeature nonTerminals g')
 putStrLn $ show gi
 putStrLn ""
 putStrLn . show . buildTable $ g
\end{code}