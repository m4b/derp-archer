\begin{code}
module BadHygiene(computeReachable,
                  eliminateUnreachable,
                  computeGenerating,
                  eliminateNonGenerating,
                  eliminateUseless,
                  isEmptyGrammar,
                  runTests,
                  doTestsPass) where

import ContextFreeGrammar
import qualified Data.Set as S
import Filterable
import ScanAndParse
import Test.HUnit

{-BEGIN CLEANING FUNCTIONS-}

{-| 
  computeReachable finds the Set of all Non Terminals of a Grammar that
  can be reached from the start node.
 -}  
computeReachable :: Ord nt => Grammar nt t -> S.Set nt
computeReachable [] = S.empty
computeReachable ps = go (S.singleton . nonterminal . head $ ps) (concat . replicate (length ps) $ ps) where
  go marked [] = marked
  go marked ((Production nt rhs):prs) = if S.member nt marked
                                        then go marked' prs
                                        else go marked prs
    where marked' = S.union marked . S.fromList . nonTerminals $ rhs

{-|
  eliminateUnreachable removes all unreachable Non Terminals from
  a Grammar.
 -}
eliminateUnreachable :: Ord nt => Grammar nt t -> Grammar nt t
eliminateUnreachable g = cleanGrammar where
  reachable = computeReachable $ g
  --unnecessary?  By definition, the unreachable non-terminals cannot be in any
  --other production list.
  --cleanProductions = Filterable.filter (`S.member` reachable) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt reachable) g

{-|
  computeGenerating finds the Set of all Non Terminals of a Grammar
  that can produce a string of Terminals.
 -}
computeGenerating :: (Ord nt, Ord t) => Grammar nt t -> S.Set nt
computeGenerating [] = S.empty
computeGenerating ps = go S.empty (concat . replicate (length ps) $ ps) where
  allTerms = S.fromList . concatMap (terminals . rhs) $ ps
  go markedNT [] = markedNT
  go markedNT ((Production nt rhs):prs) = if (all (`S.member` allTerms) . terminals $ rhs) &&
                                             (all (`S.member` markedNT) . nonTerminals $ rhs)
                                          then go (S.insert nt markedNT) prs 
                                          else go markedNT prs

{-|
  eliminateNonGenerating removes all non Generating Non Terminals from
  a Grammar.
 -}
eliminateNonGenerating :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateNonGenerating g = cleanGrammar where
  generating = computeGenerating g
  cleanProductions = Filterable.filter (`S.member` generating) g
  cleanGrammar = Prelude.filter (\(Production nt rhs) -> S.member nt generating) cleanProductions

{-|
  eliminateUseless removes all non Generating and unreachable Non Terminals
  from a Grammar.
 -}
eliminateUseless :: (Ord nt, Ord t) => Grammar nt t -> Grammar nt t
eliminateUseless = eliminateUnreachable . eliminateNonGenerating

{-|
  isEmptyGrammar determines if a Grammar will produce any strings
  at all.
 -}
isEmptyGrammar :: (Ord t, Ord nt) => Grammar nt t -> Bool
isEmptyGrammar [] = True
isEmptyGrammar g = not . elem nt . map nonterminal $ g' where
  g' = eliminateNonGenerating g
  (Production nt _) = head g   

{-END CLEANING FUNCTIONS-}
 
{-BEGIN TEST FUNCTIONS-}

testComputeReachable1 = makeTest "testComputeReachable1" "for computeReachable" e a where
  e = S.fromList ["A","B","S"]
  a = computeReachable simpleGrammar
  
testComputeReachable2 = makeTestM "testComputeReachable2" 
                                  "tests\\test1.txt" 
                                  "for computeReachable with test1" 
                                  (S.singleton "A")
                                  computeReachable
  
testEliminateUnreachable1 = makeTest "testEliminateUnreachable1" "for eliminateUnreachable" e a where
  e = [s,s',b,a'] where
    s = Production "S" (NonT "A" (NonT "B" Empty))
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
    a' = Production "A" (Term "a" (NonT "A" Empty))
  a = eliminateUnreachable simpleGrammar
  
testEliminateUnreachable2 = makeTestM "testEliminateUnreachable2" 
                                      "tests\\test1.txt" 
                                      "for eliminateUnreachable with test1" 
                                      [Production "A" (Term "a" Empty)]
                                      eliminateUnreachable
  
testComputeGenerating1 = makeTest "testComputeGenerating1" "for computeGenerating" e a where
  e = S.fromList ["B","C","S"]
  a = computeGenerating simpleGrammar
  
testComputeGenerating2 = makeTestM "testComputeGenerating2" 
                                    "tests\\test1.txt" 
                                    "for computeGenerating with test1" 
                                    (S.fromList ["A","B","D","C"])
                                    computeGenerating
  
testEliminateNonGenerating1 = makeTest "testEliminateNonGenerating1" "for eliminateNonGenerating" e a where
  e = [s,s',b,c] where
    s = Production "S" (NonT "B" Empty)
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
    c = Production "C" (Term "d" Empty)
  a = eliminateNonGenerating simpleGrammar
  
testEliminateNonGenerating2 = makeTestM "testEliminateNonGenerating2" 
                                        "tests\\test1.txt" 
                                        "for eliminateNonGenerating with test1" 
                                        e
                                        eliminateNonGenerating where
  e = [a,b,c,d,b',b'',c'] where
    a = Production "A" (Term "a" Empty)
    b = Production "B" (Term "b" Empty)
    c = Production "C" (Term "a" (NonT "D" Empty))
    d = Production "D" (NonT "B" (NonT "C" (NonT "A" Empty)))
    b' = Production "B" Empty
    b'' = Production "B" (NonT "C" Empty)
    c' = Production "C" (NonT "B" Empty)
  
testEliminateUseless1 = makeTest "testEliminateUseless1" "for eliminateUseless" e a where
  e = [s,s',b] where
    s = Production "S" (NonT "B" Empty)
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
  a = eliminateUseless simpleGrammar
  
testEliminateUseless2 = makeTestM "testEliminateUseless2" 
                                  "tests\\test1.txt" 
                                  "for eliminateUseless with test1" 
                                  [Production "A" (Term "a" Empty)]
                                  eliminateUseless
  
testIsEmptyGrammar1 = makeTest "testIsEmptyGrammar1" "for isEmptyGrammar" e a where
  e = False
  a = isEmptyGrammar simpleGrammar
  
testIsEmptyGrammar2 = makeTestM "testIsEmptyGrammar2" 
                                "tests\\test1.txt" 
                                "for isEmptyGrammar with test1" 
                                False
                                isEmptyGrammar

makeTestM :: (Eq a, Show a) => String -> FilePath -> String -> a -> (Grammar String String -> a) -> Test  
makeTestM name file forF e f = TestLabel name . TestCase $ do
  grammar <- fmap sparse . readFile $ file
  assertEqual forF e (f grammar)

makeTest :: (Eq a, Show a) => String -> String -> a -> a -> Test
makeTest s forS expected = TestLabel s . TestCase . assertEqual forS expected

tests = TestList [testComputeReachable1,
                  testComputeReachable2,
                  testEliminateUnreachable1,
                  testEliminateUnreachable2,
                  testComputeGenerating1,
                  testComputeGenerating2,
                  testEliminateNonGenerating1,
                  testEliminateNonGenerating2,
                  testEliminateUseless1,
                  testEliminateUseless2,
                  testIsEmptyGrammar1,
                  testIsEmptyGrammar2]

runTests :: IO Counts
runTests = runTestTT tests

doTestsPass :: IO Bool
doTestsPass = do
  counts <- runTests
  let errs = errors counts
      fails = failures counts
  return $ (errs == 0) && (fails == 0)
 
simpleGrammar :: Grammar String String
simpleGrammar = [s,s',b,a,c] where
  s = Production "S" (NonT "A" (NonT "B" Empty))
  s' = Production "S" (Term "x" Empty)
  b = Production "B" (Term "b" Empty)
  a = Production "A" (Term "a" (NonT "A" Empty))
  c = Production "C" (Term "d" Empty)
  
{-END TEST FUNCTIONS-}  
\end{code}