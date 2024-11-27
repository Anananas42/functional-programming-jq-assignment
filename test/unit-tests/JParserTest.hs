module JParserTest (jParserTests) where

import Jq.JParser (parseJSON)
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

jParserTests :: TestTree
jParserTests = testGroup"JParser tests"[
    testCase "nullTest" $ "null" `parseTo` JNull,
    testCase "failure" $ fail "tnull"]

parseTo :: String -> JSON -> Assertion
parseTo s j = case parse parseJSON s of
    [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
    [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
    x -> assertFailure $ "Parsing failed: " ++ show x

fail :: String -> Assertion
fail s = case parse parseJSON s of
    [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
    _ -> return ()
