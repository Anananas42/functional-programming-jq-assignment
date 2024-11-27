{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Main where

import           Control.Monad
import           Control.DeepSeq
import           Data.Either                          ( isLeft, isRight, fromRight )
import           Data.List
import           GHC.Generics (Generic, Generic1)
import           System.Exit
import           Test.Tasty                           ( defaultMain, testGroup )
import           Test.Tasty.QuickCheck                ( testProperty )
import           Test.QuickCheck

import           Jq.Filters                           ( Filter(..)
                                                      , filterIdentitySC
                                                      , filterStringIndexingSC
                                                      , filterPipeSC
                                                      , filterCommaSC )

import           Jq.Json                              ( JSON(..)
                                                      , jsonNullSC
                                                      , jsonNumberSC
                                                      , jsonStringSC
                                                      , jsonBoolSC
                                                      , jsonArraySC
                                                      , jsonObjectSC )

import           Jq.Compiler                          ( compile
                                                      , run )

{--
  It can be that one (or both) of these two derivation fail.
  Especially if you introduce some non-trivial constructors
  or if your definition of filter is mutually recursive with
  some other definition.
  This doesn't necessarily mean that you're doing anything wrong.
  You can try fixing it yourself by adding
  `deriving instance Generic X` and
  `deriving instance NFData X` below for the missing classes.
  In case this doesn't work reach out to the course team.
--}
deriving instance Generic JSON
deriving instance NFData JSON

instance Arbitrary JSON where
    arbitrary = do
        n <- arbitrary :: Gen Int
        s <- arbitrary :: Gen String
        b <- arbitrary :: Gen Bool
        xs <- frequency [ (1, return []), (5, do { x <- arbitrary; return [x] })]
        ys <- frequency [ (1, return []), (5, do { x <- arbitrary; s <- arbitrary; return [(s, x)] })]

        elements [ jsonNullSC, jsonNumberSC n, jsonStringSC s, jsonBoolSC b, jsonArraySC xs, jsonObjectSC ys ]


deriving instance Generic Filter
deriving instance NFData Filter

instance Arbitrary Filter where
    arbitrary = do
        id  <- arbitrary :: Gen String
        f   <- arbitrary
        frequency [ (5, return filterIdentitySC), (5, return (filterStringIndexingSC id)), (1, return (filterPipeSC f f)), (1, return (filterCommaSC f f)) ]

main = defaultMain tests

tests = testGroup "Week 4 tests" [
    testGroup "Constructors are defined" [
        testProperty "Constructor for identity computes" prop_computes_identity
      , testProperty "Constructor for indexing computes" prop_computes_indexing
      , testProperty "Constructor for pipe computes" prop_computes_pipe
      , testProperty "Constructor for comma computes" prop_computes_comma]
  , testGroup "Identity" [
        testProperty "Identity functionality" prop_identity]
  , testGroup "Indexing" [
        testProperty "Indexing existing keys" prop_index_existent
      , testProperty "Indexing non-existing keys" prop_index_non_existent
      , testProperty "Indexing null" prop_index_null
      , testProperty "Indexing numbers" prop_index_number
      , testProperty "Indexing strings" prop_index_string
      , testProperty "Indexing booleans" prop_index_bool
      , testProperty "Indexing arrays" prop_index_array]
  , testGroup "Pipe" [
        testProperty "Pipe with identity on the right" prop_pipe_identity_right
      , testProperty "Pipe with identity on the left" prop_pipe_identity_left
      , testProperty "An error on the left of the pipe leads to an error" prop_pipe_carries_error
      , testProperty "Using a pipe to do nested indexing" prop_pipe_index
      , testProperty "Multiple pipes with an error in the first one" prop_pipe_err_first
      , testProperty "Multiple pipes with an error in the last one" prop_pipe_err_last]
  , testGroup "Comma" [
        testProperty "Comma with identical filters leads to duplicate output" prop_comma_duplicates
      , testProperty "An error on the left of the comma leads to an error" prop_comma_carries_error
      , testProperty "Comma with two identities duplicates the input" prop_comma_identity
      , testProperty "Comma with a failing left index fails" prop_comma_index_error_l
      , testProperty "Comma with a failing right index fails" prop_comma_index_error_r
      , testProperty "Comma with index" prop_comma_index
      , testProperty "Comma with pipe" prop_comma_pipe]
    ]

prop_computes_identity         = total $ filterIdentitySC
prop_computes_indexing id      = total $ filterStringIndexingSC id
prop_computes_pipe f g         = total $ filterPipeSC f g
prop_computes_comma f g        = total $ filterCommaSC f g

prop_identity j                = run (compile filterIdentitySC) j === Right [j]

prop_index_existent s j        = run (compile $ filterStringIndexingSC s) (jsonObjectSC [(s, j)]) === Right [j]
prop_index_non_existent s t j  = s /= t ==>
                                 run (compile $ filterStringIndexingSC t) (jsonObjectSC [(s, j)]) === Right [jsonNullSC]
prop_index_null s              = run (compile $ filterStringIndexingSC s) jsonNullSC === Right [jsonNullSC]
prop_index_number n s          = isLeft $ run (compile $ filterStringIndexingSC s) (jsonNumberSC n)
prop_index_string s t          = isLeft $ run (compile $ filterStringIndexingSC s) (jsonStringSC t)
prop_index_bool b s            = isLeft $ run (compile $ filterStringIndexingSC s) (jsonBoolSC b)
prop_index_array j s           = isLeft $ run (compile $ filterStringIndexingSC s) (jsonArraySC [j])

prop_pipe_identity_right f j   = run (compile $ filterPipeSC f filterIdentitySC) j === compile f j
prop_pipe_identity_left f j    = run (compile $ filterPipeSC filterIdentitySC f) j === compile f j
prop_pipe_carries_error f g j  = isLeft (run (compile f) j) ==>
                                 isLeft (run (compile $ filterPipeSC f g) j)
prop_pipe_index k1 k2 v        = run (compile $ filterPipeSC (filterStringIndexingSC k1) (filterStringIndexingSC k2)) (jsonObjectSC [(k1, jsonObjectSC [(k2, v)])]) === Right [v]
prop_pipe_err_first k1 k2 k3   = isLeft $ run (compile $ filterPipeSC (filterStringIndexingSC k1) (filterPipeSC (filterStringIndexingSC k2)  (filterStringIndexingSC k3) )) (jsonNumberSC 42)
prop_pipe_err_last k1 k2 k3    = isLeft $ run (compile $ filterPipeSC (filterStringIndexingSC k1) (filterPipeSC (filterStringIndexingSC k2)  (filterStringIndexingSC k3) )) (jsonObjectSC [(k1, jsonObjectSC [(k2, jsonNumberSC 42)])])

prop_comma_duplicates f j      = let res = run (compile f) j in
                                   isRight res ==>
                                   run (compile $ filterCommaSC f f) j === Right (fromRight [] res ++ fromRight [] res)
prop_comma_carries_error f g j = isLeft (run (compile f) j) ==>
                                 isLeft (run (compile $ filterCommaSC f g) j)
prop_comma_identity j          = run (compile $ filterCommaSC filterIdentitySC filterIdentitySC) j === Right [j, j]
prop_comma_index_error_l k     = isLeft $ run (compile $ filterCommaSC (filterStringIndexingSC k) (filterIdentitySC)) (jsonNumberSC 42)
prop_comma_index_error_r k     = isLeft $ run (compile $ filterCommaSC (filterIdentitySC) (filterStringIndexingSC k)) (jsonNumberSC 42)
prop_comma_index k1 k2 v1 v2   = k1 /= k2  ==>
                               run (compile $  filterCommaSC (filterStringIndexingSC k1) (filterStringIndexingSC k2)) (jsonObjectSC [(k1, v1), (k2, v2)])
                               === Right [v1, v2]
prop_comma_pipe k1 k2 k3 v1 v2 = k1 /= k2 && k1 /= k3 && k2 /= k3 ==>
                               run (compile $ filterPipeSC (filterCommaSC (filterStringIndexingSC k1) (filterStringIndexingSC k2)) (filterStringIndexingSC k3)) (jsonObjectSC [(k1, jsonObjectSC [(k3, v1)]), (k2, jsonObjectSC [(k3, v2)])])
                               === Right [v1, v2]
