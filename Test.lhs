\begin{code}
{-BEGIN TEST FUNCTIONS-}

module Test (     runTests,
                  doTestsPass) where

import Test.HUnit
import BadHygiene

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