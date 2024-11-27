module CompileTest (compileTests) where

import Jq.CParser (parseFilter)
import Jq.Compiler (compile, run)
import Jq.Filters (Filter (..))
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

compileTests :: TestTree
compileTests = testGroup "Compile tests" [
    testCase "identityNullTest" $ (Identity, JNull) `compileTo` [JNull]]

compileTo :: (Filter, JSON) -> [JSON] -> Assertion
compileTo (f, j) o = case run (compile f) j of
  Left s -> assertFailure $ "Compilation failed with:\n" ++ s
  Right v -> assertEqual ("Expected:\n" ++ show o ++ "\ngot:\n" ++ show v) o v

fail :: Filter -> JSON -> Assertion
fail f j = case run (compile f) j of
  Right v -> assertFailure $ "Compilation should fail but succeeded with:\n" ++ show v
  _ -> return ()
