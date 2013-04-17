\begin{code}
module First(first) where
import ContextFreeGrammar
import Control.Applicative
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

first :: (Ord nt, Ord t) => Grammar nt t -> M.Map nt (S.Set (Terminal t))
first g = firsts . execState state . FS M.empty . nullable $ g where
  state = mapM first' . concat . replicate (length g) $ g
  --state = mapM first' g


data FirstState nt t = FS {
  firsts :: M.Map nt (S.Set (Terminal t)),
  nulls :: S.Set nt
}

type Environment nt t a = State (FirstState nt t) a



first' (Production nt rhs) = do
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
  


firstRHS :: (Ord nt, Ord t) => RHS nt t -> Environment nt t [S.Set (Terminal t)]
firstRHS Empty = return []

firstRHS (Term y _) = firstT y >>= \s -> return [s]
   
firstRHS (NonT y ys) = do
  nlls <- gets nulls
  case S.member y nlls of
    True -> do
      set <- getFirsts y
      sets <- firstRHS ys
      return (set:sets)
    False -> getFirsts y >>= \s -> return [s]

firstT :: Ord t => t -> Environment nt t (S.Set (Terminal t))
firstT t = return . S.singleton . Terminal $ t

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
                       "tests\\ir.txt"
                       "for first with ir"
                       expected
                       first where
  expected = M.fromList [p,is,i]
  p = ("Program",S.fromList [Epsilon,Terminal "comma"])
  is = ("Instructions",S.fromList [Epsilon,Terminal "comma"])
  i = ("Instruction",S.fromList [Epsilon])

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
\end{code}