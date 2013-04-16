\begin{code}
module First where

import ContextFreeGrammar
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Nullable
import ScanAndParse
import qualified Data.Set as S
import Test.HUnit

first :: (Ord nt, Ord t) => Grammar nt t -> M.Map nt (S.Set (Terminal t))
first g = M.fromList . map (first' g) $ concat . replicate (length g) $ g

first' g (Production x _) = (x, first'' g x)

first'' g x = set where
  (Production _ rhs) = fromJust . find ((== x) . nonterminal) $ g
  set = case rhs of
          Empty -> S.singleton Epsilon
          Term t _ -> S.singleton (Terminal t)
          (NonT y rhs) -> S.union sety setrhs where
            sety = S.delete Epsilon $ first'' g y
            setrhs = if S.member y nulls
                     then combine g nulls S.empty rhs else S.empty
            nulls = nullable g
            
          
combine :: (Ord nt, Ord t) => Grammar nt t -> S.Set nt -> S.Set (Terminal t) -> RHS nt t -> S.Set (Terminal t)
combine _ _ acc Empty = acc
combine _ _ acc (Term t _) = S.insert (Terminal t) acc
combine g nulls acc (NonT y ys) = if S.member y nulls
  then combine g nulls (S.union fy acc) ys
  else S.union fy acc where
    fy = S.delete Epsilon $ first'' g y
  
{-- BEGIN TESTS --}
makeTestM :: (Eq a, Show a) => String -> FilePath -> String -> a -> (Grammar String String -> a) -> Test  
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
                                          Terminal "a"]),
                         ("C",S.fromList [Terminal "a",
                                          Terminal "b"]),
                          ("D",S.fromList [Terminal "a",
                                           Terminal "b"])]

tests = TestList [testFirst]
                                           
runTests :: IO Counts
runTests = runTestTT tests

doTestsPass :: IO Bool
doTestsPass = do
  counts <- runTests
  let errs = errors counts
      fails = failures counts
  return $ (errs == 0) && (fails == 0)
\end{code}