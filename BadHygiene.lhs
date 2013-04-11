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
computeGenerating ps = go S.empty (S.fromList . concatMap (terminals . rhs) $ ps) (concat . replicate (length ps) $ ps) where
  go markedNT _ [] = markedNT
  go markedNT markedT ((Production nt rhs):prs) = if (all (`S.member` markedT) . terminals $ rhs) &&
                                                     (all (`S.member` markedNT) . nonTerminals $ rhs)
   then go (S.insert nt markedNT) markedT prs else go markedNT markedT prs

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
  
testEliminateUnreachable1 = makeTest "testEliminateUnreachable1" "for eliminateUnreachable" e a where
  e = [s,s',b,a'] where
    s = Production "S" (NonT "A" (NonT "B" Empty))
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
    a' = Production "A" (Term "a" (NonT "A" Empty))
  a = eliminateUnreachable simpleGrammar
  
testComputeGenerating1 = makeTest "testComputeGenerating1" "for computeGenerating" e a where
  e = S.fromList ["B","C","S"]
  a = computeGenerating simpleGrammar
  
testEliminateNonGenerating1 = makeTest "testEliminateNonGenerating1" "for eliminateNonGenerating" e a where
  e = [s,s',b,c] where
    s = Production "S" (NonT "B" Empty)
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
    c = Production "C" (Term "d" Empty)
  a = eliminateNonGenerating simpleGrammar
  
testEliminateUseless1 = makeTest "testEliminateUseless1" "for eliminateUseless" e a where
  e = [s,s',b] where
    s = Production "S" (NonT "B" Empty)
    s' = Production "S" (Term "x" Empty)
    b = Production "B" (Term "b" Empty)
  a = eliminateUseless simpleGrammar
  
testIsEmptyGrammar1 = makeTest "testIsEmptyGrammar1" "for isEmptyGrammar" e a where
  e = False
  a = isEmptyGrammar simpleGrammar

makeTest s forS expected = TestLabel s . TestCase . assertEqual forS expected

tests = TestList [testComputeReachable1,
                  testEliminateUnreachable1,
                  testComputeGenerating1,
                  testEliminateNonGenerating1,
                  testEliminateUseless1,
                  testIsEmptyGrammar1]

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