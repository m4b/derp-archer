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

import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import ScanAndParse
import BadHygiene

type Table = M.Map (String,String) (Production String String)

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
getNewStart :: Grammar String String -> Grammar String String
getNewStart [] = error "getNewStart run on empty grammar --- a new low point"
getNewStart g@(Production nt rhs:ps) = 
            (Production (nt ++ "'") (NonT nt (Term "$" Empty))):g


isRHSNullable :: Ord a => S.Set a -> RHS a t -> Bool
isRHSNullable _ Empty = True
isRHSNullable _ (Term t _) = False
isRHSNullable m (NonT nt rhs) = 
              if S.member nt m then 
                 isRHSNullable m rhs
              else False

firstRHS _ Empty = S.empty
firstRHS i (Term t rhs) = S.singleton t
firstRHS i (NonT nt rhs) = 
         if (S.member nt (nulls i)) then 
            S.union firstsnt (firstRHS i rhs)
         else 
            firstsnt
  where firstsnt = ((firsts i) M.! nt)

data GrammarInfo = GI {
  firsts :: M.Map String (S.Set String),
  follows :: M.Map String (S.Set String),
  nulls :: S.Set String
} deriving Show


\end{code}

A production N \(\to \alpha \) is in the table (N,\emph a) \emph{iff} \emph a is in first(\(\alpha\)) \(\lor\) (nullable(\(\alpha\)) \(\land\) \emph a is in follow(\(\alpha\))).  This condition is readily translateable into our code:

\begin{code}


validEntry gi (Production nterm alpha) term = firstalpha || (nullablea && follown)
  where 
        firstalpha = S.member (term) (firstRHS gi alpha)
        nullablea = isRHSNullable (nulls gi) alpha
        follown = S.member (term) ((follows gi) M.! nterm)

\end{code}

We'll also need some conversion from the Terminal data types used by the first and follow functions:

\begin{code}

toTerminal [] = []
toTerminal ("$":ss) = EOF:toTerminal ss
toTerminal (s:ss) = Terminal s:toTerminal ss

fromTerminal' :: Terminal String -> String
fromTerminal' Epsilon      = ""
fromTerminal' EOF          = "$"
fromTerminal' (Terminal t) = t
fromTerminalSet ts = S.map fromTerminal' ts

\end{code}

Lastly, we build a table for an LL(1) parser, returning nothing if there is more than one production per entry in the table.

\begin{code}

buildTable' :: GrammarInfo -> Grammar String String -> [String] -> Table -> Maybe Table
buildTable' _ [] _ acc = Just acc 
buildTable' gi (p@(Production nt _):ps) terms acc = case fromList acc kvs of
  Nothing -> Nothing 
  Just a -> buildTable' gi ps terms a
  where
    valids = 
           map fst . L.filter snd . map (\t -> (t,validEntry gi p t)) $ terms
    kvs = map (\v -> ((nt,v),p)) valids

fromList :: Table -> 
         [((String,String),Production String String)] -> Maybe Table
fromList acc [] = Just acc
fromList acc ((k@(nt,t),p):ks) = case M.lookup k acc of
  Nothing -> fromList (M.insert k p acc) ks
  Just _ -> Nothing

buildTable grammar = 
           let terms = getFeature terminals grammar in
           let gi = GI (M.map fromTerminalSet $ first grammar) (M.map fromTerminalSet $ follow grammar) (nullable grammar) in
           let table = buildTable' gi grammar terms M.empty in
           table

\end{code}