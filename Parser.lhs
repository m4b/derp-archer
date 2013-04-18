\subsection{Table-driven Parser module}

In this section we provide a simple table-driven parser function, |parseWithTable|.  It takes a start non-terminal symbol, a table created in the |Table| module, an input string, and returns a boolean indicating whether the string was successfully parsed or not.

Since we are just creating a parser, but do not know the intended use of the parser, we decided to simply return a boolean.  If necessary, it could be easily modified for other accomodations.
n
Lastly, we decided to emit Haskell code as a module, to run on a given text file as input for that the context free grammar it was generated for.


\begin{code}

module Parser where

import ContextFreeGrammar
import Data.Char
import System.Environment

import qualified Data.Map as M

import ScanAndParse
import BadHygiene
import Table


getStart :: Grammar String String -> String
getStart ((Production nt _):_) = nt

rhsString = 
          "rhsToList :: RHS String String -> [String]\n" ++
          "rhsToList Empty = []\n" ++
          "rhsToList (Term t rhs) = t : rhsToList rhs\n" ++
          "rhsToList (NonT nt rhs) = nt : rhsToList rhs"

parseWithTableString = 
     "\nparseWithTable start t s = parse' [start] s where\n" ++
     "  parse' [] cs = True\n" ++
     "  parse' (top:rest) s@(c:cs)\n" ++
     "    | isUpper (head top) = case M.lookup (top,[c]) t of\n" ++ 
     "      Nothing -> False\n" ++ 
     "      Just (Production _ rhs) -> parse' (rhsToList rhs ++ rest) s\n" ++
     "    | c == head top = parse' rest cs\n" ++
     "    | otherwise = parse' rest s\n"


generateStrong grammar table = do
               putStrLn "module LLParser (parse) where "
               putStrLn "import qualified Data.Map as M"
               putStrLn "import ContextFreeGrammar"
               putStrLn "import Data.Char"
               putStr "\ntable = M."
               putStrLn . show $ table
               putStrLn parseWithTableString
               putStrLn rhsString
               putStr $ "parse s = parseWithTable " ++ "\"" ++ (getStart grammar) ++ "\"" ++ " table " ++ "(s++\"$\")" ++ "\n"

parseTableAString =
     "\nnparseWithTable start t s = parse' [start] s where" ++
     "  parse' [] cs= True\n" ++
     "  parse' (top:rest) s@(c:cs)\n" ++
     "    | isUpper (head top) = case M.lookup (top,[c]) t of\n" ++ 
     "      Nothing -> False\n" ++ 
     "      Just (Production _ rhs) -> parse' (rhsToList rhs ++ rest) s\n" ++
     "    | c == head top = parse' rest cs\n" ++
     "    | otherwise = False\n"
               

tableA = M.fromList [
  (("R","$"),[Production {nonterminal = "R", rhs = Empty}]),
  (("R","b"),[Production {nonterminal = "R", rhs = NonT "R" (Term "b" (NonT "R" Empty))},Production {nonterminal = "R", rhs = Empty}]),
  (("R","c"),[Production {nonterminal = "R", rhs = Empty}]),
  (("T","$"),[Production {nonterminal = "T", rhs = NonT "R" Empty}]),
  (("T","a"),[Production {nonterminal = "T", rhs = Term "a" (NonT "T" (Term "c" Empty))}]),
  (("T","b"),[Production {nonterminal = "T", rhs = NonT "R" Empty}]),
  (("T","c"),[Production {nonterminal = "T", rhs = NonT "R" Empty}]),
  (("T'","$"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}]),
  (("T'","a"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}]),
  (("T'","b"),[Production {nonterminal = "T'", rhs = NonT "T" (Term "$" Empty)}])]

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
-- R -> R b R
-- R -> 

rhsToList :: RHS String String -> [String]
rhsToList Empty = []
rhsToList (Term t rhs) = t : rhsToList rhs
rhsToList (NonT nt rhs) = nt : rhsToList rhs

parseA s = parseWithTableA "T'" tableA (s++"$")

generate = undefined


\end{code}
