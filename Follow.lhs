\subsection{Follow}

In this section, we implement a function |follow| which calculates the follow set for our data structure of production grammars.

\begin{code}
module Follow(follow) where

import ContextFreeGrammar
import Control.Monad.State
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import First
import Nullable
import ScanAndParse
import Test.HUnit hiding (State)

\end{code}

A GrammarState holds data that the follow' function
requires to work.

\begin{code}

data GrammarState nt t = GS {
  grammar :: Grammar nt t,
  follows :: M.Map nt (S.Set (Terminal t)),
  firsts :: M.Map nt (S.Set (Terminal t))
}

type Environment nt t a = State (GrammarState nt t) a

\end{code}

follow is the interface function exported for general use.
Given a Grammar, follow computes the Follow Set for each
Production, and returns the Sets in a Map from a Non-terminal
to it's Follow Set.

\begin{code}

follow :: (Ord nt, Ord t) 
          => Grammar nt t 
          -> M.Map nt (S.Set (Terminal t))
follow [] = M.empty
follow g@((Production s rhs):ps) = fs where
  state = mapM follow' . concat . replicate (length g) $ g
  fs = follows . execState state . GS g initial . first $ g
  initial = M.singleton s (S.singleton EOF)

\end{code}

follow' is where the main work of the follow function is done.
For a given production, follow' will add an entry into the
GrammarState passed along.  This function is meant to be
mapM'd across the Grammar you want to compute the follow sets
of.

\begin{code}

follow' :: (Ord nt, Ord t)
           => Production nt t
           -> Environment nt t ()
follow' (Production a _) = do
  fllostate <- get
  let g = grammar fllostate
      fllow = follows fllostate
      ps = filter (elem a . nonTerminals . rhs) g
  sets <- forM ps $ \(Production x rhs) -> do
    case after a rhs of
      Empty -> return . fromMaybe S.empty . M.lookup x $ fllow
      Term t _ -> return . S.singleton . Terminal $ t
      NonT nt rest -> do
        let frstb = (firsts fllostate) M.! nt
            fllowx = case S.member Epsilon frstb of
              False -> S.empty
              True -> fromMaybe S.empty . M.lookup x $ fllow
        return $ S.union (S.delete Epsilon frstb) (fllowx)
  let s = fromMaybe S.empty (M.lookup a fllow)
      newS = S.union s (S.unions sets)
  put fllostate{follows = M.insert a newS fllow}
  
\end{code}

after is a helper function which removes all Terminals and
Non-terminals from a RHS until a specific Non-terminal is reached.
Then the rest of the RHS is returned.

\begin{code}

after :: (Eq nt) => nt -> RHS nt t -> RHS nt t
after nt Empty = Empty
after nt (Term t rhs) = after nt rhs
after nt (NonT nt2 rhs) = if nt == nt2 
                          then rhs else after nt rhs 

{-- BEGIN TESTS --}
makeTestM :: (Eq a, Show a) 
             => String 
             -> FilePath 
             -> String 
             -> a 
             -> (Grammar String String -> a) 
             -> Test  
makeTestM name file forF e f = TestLabel name . TestCase $ do
  grammar <- sparse <$> readFile file
  assertEqual forF e (f grammar)
  
testFollow = makeTestM "testFollow"
                       "tests\\39.txt"
                       "for first with 39"
                       expected
                       follow where
  expected = M.fromList [("R",S.fromList [EOF,Terminal "c"]),
                         ("T",S.fromList [EOF,Terminal "c"])]

tests = TestList [testFollow]
                                           
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