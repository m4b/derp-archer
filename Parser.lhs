\subsection{Table-driven Parser module}\label{parser}

In this section we provide a simple table-driven parser function, |parseWithTable|.  It takes a start non-terminal symbol, a table created in the |Table| module, an input string, and returns a boolean indicating whether the string was successfully parsed or not.

Since we are just creating a parser, but do not know the intended use of the parser, we decided to simply return a boolean reporting whether the parse was successful or not.  If necessary, it could be easily modified for other accomodations.

Another design decision was to emit Haskell code as a module for either strong LL(1) or non strong LL(1) parsers, to run on a given text file as input for that the context free grammar it was generated for.

For the strong LL(1) case, our parser behaves as required.  The more interesting problem was writing, in Haskell, a parser for non strong LL(1) grammers which attempt to parse in the LL(1) manner, and ``dynamically detect'' if the grammar isn't even LL(1).

Our solution ended up being a ``minor'' modification of the strong LL(1) parser; we decided that the necessary behavior for parsing a non-deterministic choice in the table was to essentially map the parser over the list, and then take the union of the solutions that the parsers recursively return.

In other words, we try all the cases, or as the handout stated: ``if we have no means to decide which right-hand side to select, we have to try them all.''

Surprisingly, by changing the helper function slightly for checking whether the leftmost symbol on the right hand side of a production matches the production rule (i.e., whether it was left recursive or not), we were able to parse a whole new class of grammars which caused our program to loop infinitely before.

In the end, our implementation for non-strong LL(1) parsers emits a module which marks it as non-strong, and parses, if it has to, in an LL(k) manner.  We would have liked to have written a parser that failed, or was unable to perform for idiosyncratic grammars, but due to a lack of time, we were at a loss on how to subvert the power of Haskell.

Another interesting feature would have been to check \emph{how} more than one entry was added to the table.  In other words, how the following logical statement was falsified: a production N \(\to \alpha \) is in the table (N,\emph a) \emph{iff} \emph a is in first(\(\alpha\)) \(\lor\) (nullable(\(\alpha\)) \(\land\) \emph a is in follow(\(\alpha\))).

There are three possible ways it can be made false: the left disjunct is false, and the left conjunct is false; the left disjunct is false, and the right conjunct is false; and the left disjunct is false and both conjuncts in the conjunction are false.

These three different possibilities correspond to bullet points on pg. 248 of the handout, and give hints at how to ``dynamically'' detect features about the given grammar.

Lastly, it would have been better for us to write actual functions which directly eliminate left recursion, and perform left factorization, rather than implementing these features dynamically, but again, we didn't have the time and opted for a more ``hackish'' approach.

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
     "    | otherwise = False\n"


generateStrong grammar table = do
               putStrLn "module LLParser (parse) where "
               putStrLn "import qualified Data.Map as M"
               putStrLn "import ContextFreeGrammar"
               putStrLn "import Data.Char"
               putStr "\ntable = M."
               putStrLn . show $ table
               putStrLn parseWithTableString
               putStrLn rhsString
               putStr $ "parse s = parseWithTable " 
                 ++ "\"" ++ (getStart grammar) ++ "\"" 
                 ++ " table " ++ "(s++\"$\")" ++ "\n"

parseTableAString =
     "\nnparseWithTable start t s = parse' [start] s where" ++
     "  parse' [] cs= True\n" ++
     "  parse' (top:rest) s@(c:cs)\n" ++
     "    | isUpper (head top) = case M.lookup (top,[c]) t of\n" ++ 
     "      Nothing -> False\n" ++ 
     "      Just (Production _ rhs) -> parse' (rhsToList rhs ++ rest) s\n" ++
     "    | c == head top = parse' rest cs\n" ++
     "    | otherwise = False\n"

parseWithTableAString = 
  "\nparseWithTableA start t s = parse' [start] s where\n" ++
  "  parse' [] cs = True\n" ++
  "  parse' _ []  = False\n" ++
  "  parse' st@(top:rest) s@(c:cs)\n" ++
  "    | isUpper (head top) = case M.lookup (top,[c]) t of\n" ++
  "      Nothing -> False\n" ++
  "      Just [Production _ rhs] -> parse' (rhsToList rhs ++ rest) s\n" ++
  "      Just ps -> or . map (helper rest s) $ ps\n" ++
  "    | c == head top = parse' rest cs\n" ++
  "    | otherwise = False\n" ++
  "  helper rest str (Production nt rhs) = \n" ++
  "    case nt == head newStack of\n" ++
  "         False -> parse' newStack str\n" ++
  "         True -> parse' (tail newStack) str\n" ++
  "    where newStack = rhsToList rhs ++ rest\n"

rhsToList :: RHS String String -> [String]
rhsToList Empty = []
rhsToList (Term t rhs) = t : rhsToList rhs
rhsToList (NonT nt rhs) = nt : rhsToList rhs

generate grammar table = do
               putStrLn "module NonLLParser (parse) where "
               putStrLn "import qualified Data.Map as M"
               putStrLn "import ContextFreeGrammar"
               putStrLn "import Data.Char"
               putStr "\ntableA = M."
               putStrLn . show $ table
               putStrLn parseWithTableAString
               putStrLn rhsString
               putStr $ "parse s = parseWithTableA " 
                 ++ "\"" ++ (getStart grammar) ++ "\"" 
                 ++ " tableA " ++ "(s++\"$\")" ++ "\n"

\end{code}
