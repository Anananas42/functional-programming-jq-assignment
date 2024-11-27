module Jq.Json where

import Numeric (showHex)
import Data.Char (toUpper, ord, isControl)
import Text.Printf

data JSON = JNull
    | JString String
    -- | JInt Int
    | JNumber Double
    | JBool Bool
    | JObject [(String, JSON)]
    | JArray [JSON]
    | JPlaceholder

addPadding :: Int -> String -> String
addPadding p s = case s of
    '{':'}': t    -> "{}" ++ addPadding p t
    '[':']': t    -> "[]" ++ addPadding p t
    '{'   :  t    -> '{' : addPadding (p+2) t
    '\n':'}':t    -> "\n" ++ replicate (p-2) ' ' ++ "}" ++ addPadding (p-2) t
    '['   :  t    -> '[' : addPadding (p+2) t
    '\n':']':t    -> "\n" ++ replicate (p-2) ' ' ++ "]" ++ addPadding (p-2) t
    '\n'  :  t    -> "\n" ++ replicate p ' ' ++ addPadding p t
    h     :  t    -> h : addPadding p t
    _             -> []

stringifyObject :: [(String, JSON)] -> String
stringifyObject o = case o of
    []   -> "{}"
    [h] -> "{\n" ++ encodeStringJSON (fst h) ++ ": " ++ stringify (snd h) ++ "\n}"
    h:t  -> "{\n" ++ encodeStringJSON (fst h) ++ ": " ++ stringify (snd h) ++ foldl (\acc x -> acc ++ ",\n" ++ show (fst x) ++ ": " ++ stringify (snd x)) "" t ++ "\n}"

stringifyArray :: [JSON] -> String
stringifyArray a = case a of
    []   -> "[]"
    [h] -> "[\n" ++ stringify h ++ "\n]"
    h:t  -> "[\n" ++ stringify h ++ foldl (\acc x -> acc ++ ",\n" ++ stringify x) "" t ++ "\n]"

printNumberScientific :: Double -> String
printNumberScientific n
  | isWholeNumber = printf "%.0f" n
  | otherwise     = show n
  where
    isWholeNumber = n == fromIntegral (round n :: Integer)

-- appendExponentPlus :: String -> String
-- appendExponentPlus n = case n of
--               'E' : '-' : xs -> "E-" ++ xs
--               'e' : '-' : xs -> "e-" ++ xs
--               'E' : xs       -> "E+" ++ xs
--               'e' : xs       -> "e+" ++ xs
--               x : xs         -> x : appendExponentPlus xs
--               []             -> []

escapeChar :: Char -> String
-- escapeChar '\"' = "\\\""
-- escapeChar '\\' = "\\\\"
-- escapeChar '\b' = "\\b"
-- escapeChar '\f' = "\\f"
-- escapeChar '\n' = "\\n"
-- escapeChar '\r' = "\\r"
-- escapeChar '\t' = "\\t"
escapeChar c
  | isControl c = "\\u" ++ replicate (4 - length hex) '0' ++ hex
  | ord c > 0xFFFF = surrogatePairToJSON (ord c)
  | otherwise = [c]
  where
    hex = showHex (ord c) ""

-- UTF-16
surrogatePairToJSON :: Int -> String
surrogatePairToJSON cp = "\\u" ++ padHex highHex ++ "\\u" ++ padHex lowHex
  where
    sub = cp - 0x10000
    high = sub `div` 0x400 + 0xD800
    low = sub `mod` 0x400 + 0xDC00
    highHex = map toUpper $ showHex high ""
    lowHex = map toUpper $ showHex low ""
    padHex h = replicate (4 - length h) '0' ++ h

encodeStringJSON :: String -> String
encodeStringJSON str = "\"" ++ concatMap escapeChar str ++ "\""

stringify :: JSON -> String
stringify j = case j of
  JNull        ->  "null"
  JString s    ->  encodeStringJSON s
  -- JInt n       ->  printNumberScientific (fromIntegral n)
  JNumber n    ->  printNumberScientific n
  JBool True   ->  "true"
  JBool False  ->  "false"
  JObject o    ->  stringifyObject o
  JArray a     ->  stringifyArray a
  JPlaceholder -> error "JPlaceholder encountered."

instance Show JSON where
  show j  = addPadding 0 (stringify j)

instance Eq JSON where
  JNull == JNull              = True
  JString s1 == JString s2    = s1 == s2
  JNumber n1 == JNumber n2    = n1 == n2
  JBool b1 == JBool b2        = b1 == b2
  JObject o1 == JObject o2    = o1 == o2
  JArray a1 == JArray a2      = a1 == a2
  _ == _                      = False

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC x = JNumber (fromIntegral x)

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject

