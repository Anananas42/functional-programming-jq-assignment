{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Jq.Filters where
import Jq.Json

data Filter = Identity
            | StringIndexing Filter           -- ["foo"] {"bar":1, "foo":2} = 2
            | StringIndexingOptional Filter   -- ["foo"]? {"bar":1, "foo":2} = 2
            | Slice (Filter, Filter)
            | ArrayIndexing Filter
            | ArrayIndexingOptional Filter
            | ValueIterator
            | ValueIteratorOptional
            | Pipe Filter Filter              -- Pass result from one filter to another
            | Comma Filter Filter             -- Compute both and append results
            | Parentheses Filter
            | Number Double
            -- | Integer Int
            | Literal String                  -- Value constructors
            | StringLiteral String
            | NestedExpression JSON Filter   -- NestedExpression (JSON empty at first, populated during compilation)
            | ArrayConstructor Filter
            | ObjectConstructor Filter
            | RecursiveDescent
            | KeyValue Filter Filter
            | IfThenElse Filter Filter Filter
            | TryCatch Filter Filter
            -- Boolean Operations
            | Eq Filter Filter
            | Neq Filter Filter
            | Lt Filter Filter
            | Lte Filter Filter
            | Gt Filter Filter
            | Gte Filter Filter
            | And Filter Filter
            | Or Filter Filter
            | Not Filter
            -- Arithmetic Operations
            | Plus Filter Filter
            | Minus Filter Filter
            | Mult Filter Filter
            | Div Filter Filter

instance Show Filter where
  show Identity                        = "."
  show (StringIndexing f)              = '.' : show f
  show (StringIndexingOptional f)      = '.' : show f ++ "?"
  show (Slice (start, end))            = show start ++ ":" ++ show end
  show (ArrayIndexing i)               = "ArrayIndexing " ++ show i
  show (ArrayIndexingOptional i)       = show i ++ "?"
  show ValueIterator                   = "ValueIterator []"
  show ValueIteratorOptional           = "ValueIteratorOptional []?"
  show (Pipe f1 f2)                    = show f1 ++ " | " ++ show f2
  show (Comma f1 f2)                   = show f1 ++ ", "  ++ show f2
  show (Parentheses f)                 = "(" ++ show f ++ ")"
  show (Number n)                      = "Number " ++ show n
  -- show (Integer n)                     = "Integer " ++ show n
  show (Literal v)                     = "Literal " ++ show v
  show (StringLiteral s)               = "StringLiteral " ++ show s
  show (NestedExpression _ e)          = "NestedExpression " ++ show e
  show (ArrayConstructor e)            = "ArrayConstructor " ++ show e
  show (ObjectConstructor o) = "ObjectConstructor " ++ show o
  show (KeyValue k v) = "KeyValue " ++ show k ++ ", " ++ show v
  show _ = "Show implementation missing"

instance Eq Filter where
  Identity == Identity                                              = True
  StringIndexing s1 == StringIndexing s2                            = s1 == s2
  StringIndexingOptional s1 == StringIndexingOptional s2            = s1 == s2
  Slice (s1, e1) == Slice (s2, e2)                                  = s1 == s2 && e1 == e2
  ArrayIndexing l1 == ArrayIndexing l2                              = l1 == l2
  ArrayIndexingOptional l1 == ArrayIndexingOptional l2              = l1 == l2
  ValueIterator == ValueIterator                                    = True
  ValueIteratorOptional == ValueIteratorOptional                    = True
  Pipe f1a f1b == Pipe f2a f2b                                      = f1a == f2a && f1b == f2b
  Comma f1a f1b == Comma f2a f2b                                    = f1a == f2a && f1b == f2b
  Parentheses f1 == Parentheses f2                                  = f1 == f2
  Number n1 == Number n2                                            = n1 == n2
  -- Integer n1 == Integer n2                                          = n1 == n2
  Literal v1 == Literal v2                                          = v1 == v2
  StringLiteral s1 == StringLiteral s2                              = s1 == s2
  NestedExpression _ e1 == NestedExpression _ e2                    = e1 == e2
  ArrayConstructor e1 == ArrayConstructor e2                        = e1 == e2
  _ == _                                                            = False

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC s = StringIndexing $ StringLiteral s

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
