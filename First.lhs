\begin{code}
module First(first) where

import ContextFreeGrammar
import Control.Monad
import Control.Monad.State
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Nullable
import ScanAndParse
import qualified Data.Set as S
import Test.HUnit hiding (State)

\end{code}

first is the interface function exported for general use.
Given a Grammar, first computes the First Set for each
Production, and returns the Sets in a Map from a Non-terminal
to it's First Set.

\begin{code}

first :: (Ord nt, Ord t) 
         => Grammar nt t 
         -> M.Map nt (S.Set (Terminal t))
first g = firsts . execState state . FS M.empty . nullable $ g where
  state = mapM first' . concat . replicate (length g) $ g

\end{code}

The FirstState Data Type stores the map
of Sets of First Terminals that is modified
and returned at the end of a call to first.
It also stores the set of Non-terminals which
are nullable.

\begin{code}  
  
data FirstState nt t = FS {
  firsts :: M.Map nt (S.Set (Terminal t)),
  nulls :: S.Set nt
}

type Environment nt t a = State (FirstState nt t) a

\end{code}

first' does the work for the first function.
Given a production, first' will calculate
the set of first terminals and store it
in the implicit FirstState.

\begin{code}

first' :: (Ord nt, Ord t) => Production nt t -> Environment nt t ()
first' (Production nt _) = do
  fs <- get
  let mp = firsts fs
  case rhs of
    Empty -> case M.lookup nt mp of
      Nothing -> put fs{firsts = M.insert nt (S.singleton Epsilon) mp}
      Just _ -> put fs{firsts = M.adjust (S.insert Epsilon) nt mp}
    _ -> do
      sets <- firstRHS rhs
      let s = fromMaybe S.empty (M.lookup nt mp)
      put fs{firsts = M.insert nt (S.unions (s:sets)) mp}

\end{code}

firstRHS is a helper function which, given a RHS
will return the first sets of every terminal and 
non-terminal until a non nullable terminal/non-terminal
is found.

\begin{code}

firstRHS :: (Ord nt, Ord t) 
            => RHS nt t 
            -> Environment nt t [S.Set (Terminal t)]
firstRHS Empty = return []
firstRHS (Term y _) = return [S.singleton . Terminal $ y]  
firstRHS (NonT y ys) = do
  nlls <- gets nulls
  case S.member y nlls of
    True -> do
      set <- getFirsts y
      sets <- firstRHS ys
      return (set:sets)
    False -> (:[]) <$> getFirsts y
    
\end{code}

getFirsts is a helper function which given a
Non-terminal will return its current first set.

\begin{code}

getFirsts :: Ord nt => nt -> Environment nt t (S.Set (Terminal t))   
getFirsts nt = do
  set <- (M.lookup nt) <$> gets firsts
  case set of
    Nothing -> return S.empty
    Just set -> return set

{-- BEGIN TESTS --}
makeTestM :: (Eq a, Show a) 
             => String 
             -> FilePath 
             -> String 
             -> a 
             -> (Grammar String String -> a) 
             -> Test  
makeTestM name file forF e f = TestLabel name . TestCase $ do
  grammar <- fmap sparse . readFile $ file
  assertEqual forF e (f grammar)
  
testFirst = makeTestM "testFirst"
                      "tests\\test1.txt"
                      "for first with test1"
                      expected
                      first where
  expected = M.fromList [("A",S.singleton . Terminal $ "a"),
                         ("B",S.fromList [Terminal "b", 
                                          Terminal "a",
                                          Epsilon]),
                         ("C",S.fromList [Terminal "a",
                                          Terminal "b",
                                          Epsilon]),
                          ("D",S.fromList [Terminal "a",
                                           Terminal "b",
                                           Epsilon])]
                                           
testFirst2 = makeTestM "testFirst2"
                       "tests\\39.txt"
                       "for first with 39"
                       expected
                       first where
  expected = M.fromList [("T",S.fromList [Epsilon,Terminal "a",Terminal "b"]),
                         ("R",S.fromList [Epsilon,Terminal "b"])]

tests = TestList [testFirst,
                  testFirst2]
                                           
runTests :: IO Counts
runTests = runTestTT tests

doTestsPass :: IO Bool
doTestsPass = do
  counts <- runTests
  let errs = errors counts
      fails = failures counts
  return $ (errs == 0) && (fails == 0)
{-- END TESTS --}
\end{code}