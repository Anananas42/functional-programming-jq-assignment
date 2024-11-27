{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.List
import           GHC.Generics                     (Generic, Generic1)
import           System.Exit
import           Test.Tasty                       (defaultMain, testGroup)
import           Test.Tasty.QuickCheck            (testProperty)
import           Test.QuickCheck

import           Jq.Json                              as Json

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

arbitrary' :: Int -> Gen JSON
arbitrary' 0 = frequency [ (1, arbitraryNull), (10, arbitraryNumber), (10, arbitraryString), (1, arbitraryBool) ]
arbitrary' x = frequency [ (1, arbitraryNull), (10, arbitraryNumber), (10, arbitraryString), (1, arbitraryBool), (40, arbitraryArray x), (40, arbitraryObject x)]

arbitraryNull   = return $ jsonNullSC
arbitraryNumber = choose (-4294967296, 4294967295) >>= return . jsonNumberSC
arbitraryString = choose (2, 20) >>= \n -> replicateM n (fmap chr $ choose (0,127)) >>= return . jsonStringSC
arbitraryBool   = arbitrary >>= return . jsonBoolSC

arbitraryArray :: Int -> Gen JSON
arbitraryArray 0 = return $ jsonArraySC []
arbitraryArray x = frequency [
                       (1, return $ jsonArraySC []),
                       (1, choose (1,4) >>= \x -> replicateM x (arbitrary' (x-1)) >>= return . jsonArraySC)
                   ]

arbitraryObject :: Int -> Gen JSON
arbitraryObject 0 = return $ jsonObjectSC []
arbitraryObject x = frequency [
                       (1, return $ jsonObjectSC []),
                       (1, do {
                                x <- choose (1, 4);
                                xs <- replicateM x $ choose (2,20) >>= \x -> replicateM x arbitrary;
                                ys <- replicateM x $ arbitrary' (x-1);
                                return $ jsonObjectSC (zip xs ys)
                           })
                    ]

genSafeChar :: Gen Char
genSafeChar = elements (['a'..'z'] ++ ['A'.. 'Z'] ++ ['0'..'9'])

genSafeString :: Gen String
genSafeString = listOf genSafeChar

instance Arbitrary JSON where
    arbitrary = arbitrary' 10

main = defaultMain tests

prop_computes_null  = total $ jsonNullSC
prop_computes_number n = total $ jsonNumberSC n
prop_computes_string s = total $ jsonStringSC s
prop_computes_bool b = total $ jsonBoolSC b
prop_computes_array xs = total $ jsonArraySC xs
prop_computes_object xs = total $ jsonObjectSC xs

prop_refl :: JSON -> Bool
prop_refl c = c == c

prop_null_not_equals_number n  = jsonNullSC /= jsonNumberSC n
prop_null_not_equals_string s  = jsonNullSC /= jsonStringSC s
prop_null_not_equals_bool   b  = jsonNullSC /= jsonBoolSC b
prop_null_not_equals_array  xs = jsonNullSC /= jsonArraySC xs
prop_null_not_equals_object xs = jsonNullSC /= jsonObjectSC xs

prop_number_not_equals_null   n    = jsonNumberSC n /= jsonNullSC
prop_number_not_equals_string n s  = jsonNumberSC n /= jsonStringSC s
prop_number_not_equals_bool   n b  = jsonNumberSC n /= jsonBoolSC b
prop_number_not_equals_array  n xs = jsonNumberSC n /= jsonArraySC xs
prop_number_not_equals_object n xs = jsonNumberSC n /= jsonObjectSC xs

prop_string_not_equals_null   s    = jsonStringSC s /= jsonNullSC
prop_string_not_equals_number s n  = jsonStringSC s /= jsonNumberSC n
prop_string_not_equals_bool   s b  = jsonStringSC s /= jsonBoolSC b
prop_string_not_equals_array  s xs = jsonStringSC s /= jsonArraySC xs
prop_string_not_equals_object s xs = jsonStringSC s /= jsonObjectSC xs

prop_bool_not_equals_null   b    = jsonBoolSC b /= jsonNullSC
prop_bool_not_equals_number b n  = jsonBoolSC b /= jsonNumberSC n
prop_bool_not_equals_string b s  = jsonBoolSC b /= jsonStringSC s
prop_bool_not_equals_array  b xs = jsonBoolSC b /= jsonArraySC xs
prop_bool_not_equals_object b xs = jsonBoolSC b /= jsonObjectSC xs

prop_array_not_equals_null   xs    = jsonArraySC xs /= jsonNullSC
prop_array_not_equals_number xs n  = jsonArraySC xs /= jsonNumberSC n
prop_array_not_equals_string xs s  = jsonArraySC xs /= jsonStringSC s
prop_array_not_equals_bool   xs b  = jsonArraySC xs /= jsonBoolSC b
prop_array_not_equals_object xs ys = jsonArraySC xs /= jsonObjectSC ys

prop_object_not_equals_null   xs    = jsonObjectSC xs /= jsonNullSC
prop_object_not_equals_number xs n  = jsonObjectSC xs /= jsonNumberSC n
prop_object_not_equals_string xs s  = jsonObjectSC xs /= jsonStringSC s
prop_object_not_equals_bool   xs b  = jsonObjectSC xs /= jsonBoolSC b
prop_object_not_equals_array  xs ys = jsonObjectSC xs /= jsonArraySC ys

prop_show_null               = show jsonNullSC == "null"
prop_show_number n           = within 10000 ( show (jsonNumberSC n) === show n)
prop_show_string             = forAll genSafeString $ \s -> show (jsonStringSC s) == '\"' : s ++ "\""
-- you might want to comment out these tests if they get in the way due to issues with
-- how you represent/print strings, encodings, etc.
-- in the end the only tests that matter are the grading ones
prop_show_string_not_print   = show (jsonStringSC "\0019")   === '\"' : "\\u0013" ++ "\""
prop_show_string_unicode     = show (jsonStringSC "\1234")   === '\"' : "\1234" ++ "\""
prop_show_escape_control     = show (jsonStringSC "\x0019")  === "\"\\u0019\""
prop_show_nonescaping_char   = show (jsonStringSC "\x1234")  === "\"\x1234\""
prop_show_unicode_high       = show (jsonStringSC "\x1D11E") === "\"\\uD834\\uDD1E\""
prop_show_unicode_high2      = show (jsonStringSC "😐")      === "\"\\uD83D\\uDE10\""

prop_show_bool_true          = show (jsonBoolSC True) == "true"
prop_show_bool_false         = show (jsonBoolSC False) == "false"
prop_show_empty_array        = show (jsonArraySC []) == "[]"
prop_show_array_one_element  = show (jsonArraySC [jsonNullSC]) == "[\n  null\n]"
prop_show_array_two_elements = show (jsonArraySC [jsonNullSC, jsonNullSC]) == "[\n  null,\n  null\n]"
prop_show_nested_array       = show (jsonArraySC [jsonArraySC [jsonNullSC, jsonNullSC], jsonNullSC]) == "[\n  [\n    null,\n    null\n  ],\n  null\n]"
prop_show_empty_object       = show (jsonObjectSC []) == "{}"
prop_show_object_one_element = show (jsonObjectSC [("key", jsonNullSC)]) == "{\n  \"key\": null\n}"
prop_show_nested_objects = show (jsonObjectSC [("key1", jsonNullSC), ("key2", jsonObjectSC [("key3", jsonNullSC), ("key4", jsonNullSC)])]) == "{\n  \"key1\": null,\n  \"key2\": {\n    \"key3\": null,\n    \"key4\": null\n  }\n}"

tests = testGroup "Week 3 tests" [
    testGroup "Constructors are defined" [
        testProperty "Constructor for null computes" prop_computes_null
      , testProperty "Constructor for numbers computes" prop_computes_number
      , testProperty "Constructor for strings computes" prop_computes_string
      , testProperty "Constructor for booleans computes" prop_computes_bool
      , testProperty "Constructor for array computes" prop_computes_array
      , testProperty "Constructor for objects computes" prop_computes_object
    ],
    testGroup "Inequality null" [
        testProperty "Inequality null number" prop_null_not_equals_number
      , testProperty "Inequality null string" prop_null_not_equals_string
      , testProperty "Inequality null bool" prop_null_not_equals_bool
      , testProperty "Inequality null array" prop_null_not_equals_array
      , testProperty "Inequality null object" prop_null_not_equals_object
    ],
    testGroup "Inequality number" [
        testProperty "Inequality number null" prop_number_not_equals_null
      , testProperty "Inequality number string" prop_number_not_equals_string
      , testProperty "Inequality number bool" prop_number_not_equals_bool
      , testProperty "Inequality number array" prop_number_not_equals_array
      , testProperty "Inequality number object" prop_number_not_equals_object
    ],
    testGroup "Inequality string" [
        testProperty "Inequality string null" prop_string_not_equals_null
      , testProperty "Inequality string number" prop_string_not_equals_number
      , testProperty "Inequality string bool" prop_string_not_equals_bool
      , testProperty "Inequality string array" prop_string_not_equals_array
      , testProperty "Inequality string object" prop_string_not_equals_object
    ],
    testGroup "Inequality bool" [
        testProperty "Inequality bool null" prop_bool_not_equals_null
      , testProperty "Inequality bool number" prop_bool_not_equals_number
      , testProperty "Inequality bool string" prop_bool_not_equals_string
      , testProperty "Inequality bool array" prop_bool_not_equals_array
      , testProperty "Inequality bool object" prop_bool_not_equals_object
    ],
    testGroup "Inequality array" [
        testProperty "Inequality array null" prop_array_not_equals_null
      , testProperty "Inequality array number" prop_array_not_equals_number
      , testProperty "Inequality array string" prop_array_not_equals_string
      , testProperty "Inequality array bool" prop_array_not_equals_bool
      , testProperty "Inequality array object" prop_array_not_equals_object
    ],
    testGroup "Inequality object" [
        testProperty "Inequality object null" prop_object_not_equals_null
      , testProperty "Inequality object number" prop_object_not_equals_number
      , testProperty "Inequality object string" prop_object_not_equals_string
      , testProperty "Inequality object bool" prop_object_not_equals_bool
      , testProperty "Inequality object array" prop_object_not_equals_array
    ],
    testGroup "Reflexivity of equality" [
        testProperty "Every JSON object is equal to itself" prop_refl
    ],
    testGroup "Show instances" [
        testProperty "Show null" prop_show_null
      , testProperty "Show number" prop_show_number
      , testProperty "Show string" prop_show_string
      , testProperty "Show string with non-printable character" prop_show_string_not_print
      , testProperty "Show string with printable unicode character" prop_show_string_unicode
      , testProperty "Show string with escaped control character" prop_show_escape_control
      , testProperty "Show string with non-escaping char" prop_show_nonescaping_char
      , testProperty "Show unicode high" prop_show_unicode_high
      , testProperty "Show unicode high 2" prop_show_unicode_high2
      , testProperty "Show true" prop_show_bool_true
      , testProperty "Show false" prop_show_bool_false
      , testProperty "Show empty array" prop_show_empty_array
      , testProperty "Show array one element" prop_show_array_one_element
      , testProperty "Show array two elements" prop_show_array_two_elements
      , testProperty "Show nested array" prop_show_nested_array
      , testProperty "Show empty object" prop_show_empty_object
      , testProperty "Show object one element" prop_show_object_one_element
      , testProperty "Show nested objects" prop_show_nested_objects
    ]]
