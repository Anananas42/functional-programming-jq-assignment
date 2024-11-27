module Main where

import Test.Tasty ( defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ( testCase, assertFailure, assertEqual, Assertion  )

import JParserTest ( jParserTests )
import CParserTest (cParserTests)
import CompileTest (compileTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "unitTests" [jParserTests, cParserTests, compileTests]
