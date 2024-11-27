{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (rights)


type JProgram a = JSON -> Either String a
-- type JProgram = Reader JSON (Either String [JSON])

stringIndexingHelper :: String -> JProgram [JSON] -- ["foo"] {"bar":1, "foo":2} = 2
stringIndexingHelper s inp = case inp of
    JObject ((h, t) : rest) -> (if h == s then Right [t] else stringIndexingHelper s (JObject rest))
    JObject []  -> Right [JNull]
    JNull       -> Right [JNull]
    JString _   -> Left ("Cannot index string with string " ++ show s)
    JNumber _   -> Left ("Cannot index number with string " ++ show s)
    JBool _     -> Left ("Cannot index boolean with string " ++ show s)
    JArray _    -> Left ("Cannot index array with string " ++ show s)
    JPlaceholder -> Left "JPlaceholder encountered."

compileStringIndexing :: Either String [JSON] -> JProgram [JSON]
compileStringIndexing e inp = case e of
    Right [JString s]     -> stringIndexingHelper s inp
    Right ((JString s):t) -> sequence $ sequence (stringIndexingHelper s inp) ++ sequence (compileStringIndexing (Right t) inp)
    _                     -> Left "Error in compileStringIndexing."

compileStringIndexingOptional :: Either String [JSON] -> JProgram [JSON] -- ["foo"] {"bar":1, "foo":2} = 2
compileStringIndexingOptional e inp = case inp of
    JObject obj  -> compileStringIndexing e (JObject obj)
    JNull        -> Right [JNull]
    JPlaceholder -> Left "JPlaceholder encountered."
    _            -> Right []


arrayIndexingHelper :: Int -> JProgram [JSON]
arrayIndexingHelper i inp
 | i < 0 = case inp of
        JArray xs ->  if (-i) < length xs then arrayIndexingHelper (length xs + i) $ JArray xs else Right [JNull]
        _ -> Left "Cannot index a value that is not an array"
 | otherwise = case (i, inp) of
        (0, JArray (h:_)) -> Right [h]
        (n, JArray (_:t)) -> arrayIndexingHelper (n-1) (JArray t)
        (_, JArray _)     -> Right [JNull]
        _                 -> Left ("Cannot index a value that is not an array: " ++ show inp)


evaluateNestedExpressionsToFilter :: Filter -> Filter
evaluateNestedExpressionsToFilter (NestedExpression ownInput (Slice (s, e))) = case (compile s ownInput, compile e ownInput) of
    (Right r1, Right r2) -> Slice (jsonToFilter r1, jsonToFilter r2)
    (Left l1, _)         -> error $ "Invalid value in a slice " ++ show l1
    (_, Left l2)         -> error $ "Invalid value in a slice " ++ show l2

evaluateNestedExpressionsToFilter (NestedExpression ownInput e) = case compile e ownInput of
    Right r         -> jsonToFilter r
    err             -> error $ show err
evaluateNestedExpressionsToFilter err = error ("Cannot evaluate nested expression to another filter with filter: " ++ show err)

jsonToFilter :: [JSON] -> Filter
jsonToFilter j = case j of
    [JNumber n]         -> Number n
    [JString s]      -> StringLiteral s
    ((JNumber n):t)     -> Comma (Number n) (jsonToFilter t)
    ((JString s):t)  -> Comma (StringLiteral s) (jsonToFilter t)
    err              -> error ("Cannot convert json to a filter: " ++ show err)

compileArrayIndexing :: Filter -> JProgram [JSON]
compileArrayIndexing f inp = case f of
    Comma h t                                 -> sequence $ sequence (compileArrayIndexing h inp) ++ sequence (compileArrayIndexing t inp)
    Number n                                  -> arrayIndexingHelper (floor n) inp
    Slice slice                               -> compileArraySlicing slice inp
    StringLiteral s                           -> stringIndexingHelper s inp
    NestedExpression ownInput e               -> compileArrayIndexing (evaluateNestedExpressionsToFilter $ NestedExpression ownInput e) inp
    e                                         -> Left $ "[compileArrayIndexing] This code should not be reached. " ++ show e

compileArrayIndexingOptional :: Filter -> JProgram [JSON]
compileArrayIndexingOptional f inp = case f of
    Comma h t                                 -> Right $ rights $ sequence (compileArrayIndexingOptional h inp) ++ sequence (compileArrayIndexingOptional t inp)
    Number  n                                 -> Right $ rights $ sequence $ arrayIndexingHelper (floor n) inp
    Slice slice                               -> Right $ rights $ sequence $ compileArraySlicing slice inp
    StringLiteral s                           -> Right $ rights $ sequence $ stringIndexingHelper s inp
    NestedExpression ownInput e               -> Right $ rights $ sequence $ compileArrayIndexingOptional (evaluateNestedExpressionsToFilter $ NestedExpression ownInput e) inp
    e                                         -> Left $ "[compileArrayIndexingOptional] This code should not be reached. " ++ show e

arraySlicingHelper :: (Int, Int) -> JProgram [JSON] -- .[1:-2]
arraySlicingHelper _ JNull = Right [JNull]
arraySlicingHelper (start, end) inp
 | start < 0 = case inp of
        JArray xs -> arraySlicingHelper (max 0 (length xs + start), end) $ JArray xs
        JString s -> arraySlicingHelper (max 0 (length s + start), end) $ JString s
        _ -> Left "Cannot slice a value that is not an array"
 | end < 0 = case inp of
        JArray xs -> arraySlicingHelper (start, max 0 (length xs + end)) $ JArray xs
        JString s -> arraySlicingHelper (start, max 0 (length s + end)) $ JString s
        _ -> Left "Cannot slice a value that is not an array"
 | end <= start = Right [JArray []]
 | otherwise = case (start, end, inp) of
    (0, 0, _) -> Right [JArray []]
    (_, _, JArray []) -> Right [JArray []]
    (0, m, JArray (h:t)) -> case arraySlicingHelper (0, m-1) (JArray t) of
                                Right [JArray xs] -> Right [JArray (h : xs)]
                                _ -> Left "This code should not be reached"
    (n, m, JArray (_:t)) -> arraySlicingHelper (n-1, m-1) (JArray t)
    (n ,m, JString s)    -> Right [JString $ take (m - n) $ drop start s]
    _ -> Left ""

compileArraySlicing :: (Filter, Filter) -> JProgram [JSON]
compileArraySlicing (start, end) inp = case (compile start inp, compile end inp) of
    (Right [JNumber s], Right [JNumber e]) -> arraySlicingHelper (floor s, floor e) inp
    _ -> Left ""

compileValueIterator :: JProgram [JSON]
compileValueIterator inp = case inp of
    JArray xs -> Right xs
    JObject o -> Right (map snd o)
    _ -> Left "Value iterator invalid for other types than array or object."

compileValueIteratorOptional :: JProgram [JSON]
compileValueIteratorOptional inp = case compileValueIterator inp of
    Right r -> Right r
    Left _  -> Right []

compilePipe :: Filter -> Filter -> JProgram [JSON] -- Pass result from one filter to another
compilePipe f1 f2 inp = case compile f1 inp of
    Left err  -> Left err
    Right r   -> compileList f2 r

compileList :: Filter -> [JSON] -> Either String [JSON]
compileList _ [] = Right []
compileList f (h:t) = case (compile f h, compileList f t) of
    (Left err, _)        -> Left err
    (_, Left err)        -> Left err
    (Right r1, Right r2) -> Right (r1 ++ r2)


compileComma :: Filter -> Filter -> JProgram [JSON] -- Compute both and append results
compileComma f1 f2 inp = case (compile f1 inp, compile f2 inp) of
    (Left err, _)          -> Left err
    (_, Left err)          -> Left err
    (Right r1, Right r2)   -> Right (r1 ++ r2)

compileParentheses :: Filter -> JProgram [JSON]
compileParentheses f inp = compile f inp

compileConstructors :: Filter -> JProgram [JSON]
compileConstructors (Literal lit) _ = case lit of
    "true" -> return [JBool True]
    "false" -> return [JBool False]
    "null" -> return [JNull]
    "[]" -> return [JArray []]
    "{}" -> return [JObject []]
    str -> return [JString str]
compileConstructors _ _ = error "Constructor not implemented "

compileArrayConstructor :: Filter -> JProgram [JSON]
compileArrayConstructor f inp = case compile f inp of
        Right xs -> Right [JArray $ combineIntoJArray xs]
        x           -> x
    where
        combineIntoJArray xs = case xs of
            [h]   -> [h]
            (h:t) -> h : combineIntoJArray t
            _     -> error "Array constructor reached code that should not be reachable"

extractTuple :: (Either String [JSON], Either String [JSON]) -> Either String ([JSON], [JSON])
extractTuple (v1, v2) = case (v1, v2) of
    (Right r1, Right r2) -> Right (r1, r2)
    (Left l1, _)         -> Left l1
    (_, Left l2)         -> Left l2

compilePlus :: Filter -> Filter -> JProgram [JSON]
compilePlus v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNull], v) -> Right v
    Right (v, [JNull]) -> Right v
    Right ([JNumber n1], [JNumber n2]) -> Right [JNumber (n1 + n2)]
    Right ([JArray n1], [JArray n2]) -> Right [JArray (n1 ++ n2)]
    Right ([JString s1], [JString s2]) -> Right [JString (s1 ++ s2)]
    Right ([JObject o1], [JObject o2]) -> Right [JObject (addObjects o1 o2)]
    _ -> error ""

addObjects :: [(String, JSON)] -> [(String, JSON)] -> [(String, JSON)]
addObjects a b = case a of
        [] -> b
        h:t -> addIfNotInOther h b ++ addObjects t b

addIfNotInOther :: (String, JSON) -> [(String, JSON)] -> [(String, JSON)]
addIfNotInOther (key, val) obj = case obj of
    [] -> [(key, val)]
    (otherKey, _):t -> if key == otherKey then [] else addIfNotInOther (key, val) t

compileMinus :: Filter -> Filter -> JProgram [JSON]
compileMinus v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JNumber (n1 - n2)]
    Right ([JArray a], [JArray b]) -> Right $ subtractArrays a b
    _ -> error ""

subtractArrays :: [JSON] -> [JSON] -> [JSON]
subtractArrays a b = case a of
        h:t -> filterElem h b ++ subtractArrays t b
        _ -> []
    where
        filterElem el arr = case arr of
            [] -> [el]
            h:t -> if el == h then [] else filterElem el t

compileMult :: Filter -> Filter -> JProgram [JSON]
compileMult v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JNumber (n1 * n2)]
    Right ([JString s], [JNumber n]) -> Right [JString (concat $ replicate (floor n) s)]
    Right ([JNumber n], [JString s]) -> Right [JString (concat $ replicate (floor n) s)]
    _ -> error ""

compileDiv :: Filter -> Filter -> JProgram [JSON]
compileDiv v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> if n2 == 0 then error "Cannot divide by zero." else Right [JNumber (n1 / n2)]
    Right ([JString s1], [JString s2]) -> error "String division not implemented"
    _ -> error ""

compileEqual :: Filter -> Filter -> JProgram [JSON]
compileEqual v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 == n2]
    Right ([JBool b1], [JBool b2]) -> Right [JBool $ b1 == b2]
    Right ([JString s1], [JString s2]) -> Right [JBool $ s1 == s2]
    Right ([JArray n1], [JArray n2]) -> error "Bool comparison eq not implemented"
    _ -> error ""

compileNotEqual :: Filter -> Filter -> JProgram [JSON]
compileNotEqual v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 /= n2]
    Right ([JBool b1], [JBool b2]) -> Right [JBool $ b1 /= b2]
    Right ([JString s1], [JString s2]) -> Right [JBool $ s1 /= s2]
    Right ([JArray n1], [JArray n2]) -> error "Bool comparison eq not implemented"
    _ -> error ""

compileLt :: Filter -> Filter -> JProgram [JSON]
compileLt v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 < n2]
    _ -> error ""

compileLte :: Filter -> Filter -> JProgram [JSON]
compileLte v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 <= n2]
    _ -> error ""

compileGt :: Filter -> Filter -> JProgram [JSON]
compileGt v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 > n2]
    _ -> error ""

compileGte :: Filter -> Filter -> JProgram [JSON]
compileGte v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNumber n1], [JNumber n2]) -> Right [JBool $ n1 >= n2]
    _ -> error ""

compileAnd :: Filter -> Filter -> JProgram [JSON]
compileAnd v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNull], _) -> Right [JBool False]
    Right (_, [JNull]) -> Right [JBool False]
    Right ([JBool b1], [JBool b2]) -> Right [JBool $ b1 && b2]
    Right ([JBool b1], _) -> Right [JBool b1]
    Right (_, [JBool b2]) -> Right [JBool b2]
    Right _ -> Right [JBool True]
    _ -> error ""

compileOr :: Filter -> Filter -> JProgram [JSON]
compileOr v1 v2 inp = case extractTuple (compile v1 inp, compile v2 inp) of
    Right ([JNull], [JNull]) -> Right [JBool False]
    Right ([JBool b1], [JNull]) -> Right [JBool b1]
    Right ([JNull], [JBool b2]) -> Right [JBool b2]
    Right ([JBool b1], [JBool b2]) -> Right [JBool $ b1 || b2]
    Right _ -> Right [JBool True]
    _ -> error ""

compileNot :: Filter -> JProgram [JSON]
compileNot v inp = case compile v inp of
    Right [JNull] -> Right [JBool True]
    Right [JBool b1] -> Right [JBool $ not b1]
    Right [] -> Right []
    Right [_] -> Right [JBool False]
    _ -> error ""

compileArithmeticOp :: Filter -> JProgram [JSON]
compileArithmeticOp op inp = case op of
    Plus v1 v2 -> compilePlus v1 v2 inp
    Minus v1 v2 -> compileMinus v1 v2 inp
    Mult v1 v2 -> compileMult v1 v2 inp
    Div v1 v2 -> compileDiv v1 v2 inp
    _ -> error "Passed filter is not an arithmetic operator."

compileBoolOp :: Filter -> JProgram [JSON]
compileBoolOp op inp = case op of
    Eq v1 v2 -> compileEqual v1 v2 inp
    Neq v1 v2 -> compileNotEqual v1 v2 inp
    Lt v1 v2 -> compileLt v1 v2 inp
    Lte v1 v2 -> compileLte v1 v2 inp
    Gt v1 v2 -> compileGt v1 v2 inp
    Gte v1 v2 -> compileGte v1 v2 inp
    And v1 v2 -> compileAnd v1 v2 inp
    Or v1 v2 -> compileOr v1 v2 inp
    Not v -> compileNot v inp
    _ -> error "Passed filter is not a boolean operator."

compileRecursiveDescent :: JSON -> [JSON]
compileRecursiveDescent inp = case inp of
    JArray xs -> JArray xs : foldl (\acc x -> acc ++ compileRecursiveDescent x) [] xs
    JObject xs -> JObject xs : foldl (\acc x -> acc ++ compileRecursiveDescent (snd x)) [] xs
    v -> [v]

compileObjectConstructor :: Filter -> JProgram [JSON]
compileObjectConstructor f inp = case evaluateKeyValues f inp of
        Right objKeyVals -> Right [JObject objKeyVals]
        Left l           -> Left l

evaluateKeyValues :: Filter -> JSON -> Either String [(String, JSON)]
evaluateKeyValues f inp = case f of
    KeyValue k v -> case (compile k inp, compile v inp) of
        (Right [JString s], Right [val]) -> Right [(s, val)]
        _ -> Left "Key value constructor not valid"
    Comma (KeyValue k v) t -> case (evaluateKeyValues (KeyValue k v) inp, evaluateKeyValues t inp) of
        (Right x, Right y) -> Right (x ++ y)
        _                  -> Left "Key value constructor not valid"
    _                        -> Left "Key value constructor not valid"

compileIfThenElse :: Either String [JSON] -> Filter -> Filter -> JProgram [JSON]
compileIfThenElse a b c inp = case a of
        Right [JBool False] -> compile c inp
        Right [JNull] -> compile c inp
        Right [_] -> compile b inp
        Right [] -> Right []
        Right (h:t) -> sequence $ sequence (compileIfThenElse (Right [h]) b c inp) ++ sequence (compileIfThenElse (Right t) b c inp)
        Left l              -> Left l

compileTryCatch :: Filter -> Filter -> JProgram [JSON]
compileTryCatch a b inp = case compile a inp of
    Right r -> Right r
    Left _  -> compile b inp

compile :: Filter -> JProgram [JSON]
compile Identity inp  = return [inp]
compile f inp = case f of
    StringIndexing e                -> compileStringIndexing (compile e inp) inp
    StringIndexingOptional e        -> compileStringIndexingOptional (compile e inp) inp
    ArrayIndexing e                 -> compileArrayIndexing e inp
    ArrayIndexingOptional e         -> compileArrayIndexingOptional e inp
    ValueIterator                   -> compileValueIterator inp
    ValueIteratorOptional           -> compileValueIteratorOptional inp
    Pipe f1 f2                      -> compilePipe f1 f2 inp
    Comma f1 f2                     -> compileComma f1 f2 inp
    Parentheses f1                  -> compileParentheses f1 inp
    Number n                        -> return [JNumber n]
    Literal lit                     -> compileConstructors (Literal lit) inp
    StringLiteral s                 -> return [JString s]
    NestedExpression ownInput e     -> compile e ownInput
    ArrayConstructor e              -> compileArrayConstructor e inp
    ObjectConstructor e             -> compileObjectConstructor e inp
    RecursiveDescent                -> Right $ compileRecursiveDescent inp
    IfThenElse a b c                -> compileIfThenElse (compile a inp) b c inp
    TryCatch a b                    -> compileTryCatch a b inp
    -- ArithmeticOp
    Plus v1 v2 -> compilePlus v1 v2 inp
    Minus v1 v2 -> compileMinus v1 v2 inp
    Mult v1 v2 -> compileMult v1 v2 inp
    Div v1 v2 -> compileDiv v1 v2 inp
    -- BoolOp
    Eq v1 v2 -> compileEqual v1 v2 inp
    Neq v1 v2 -> compileNotEqual v1 v2 inp
    Lt v1 v2 -> compileLt v1 v2 inp
    Lte v1 v2 -> compileLte v1 v2 inp
    Gt v1 v2 -> compileGt v1 v2 inp
    Gte v1 v2 -> compileGte v1 v2 inp
    And v1 v2 -> compileAnd v1 v2 inp
    Or v1 v2 -> compileOr v1 v2 inp
    Not v -> compileNot v inp
    _ -> Left "Compilation for this filter is missing."

populateExpressionsAndCompile :: Filter -> JProgram [JSON]
populateExpressionsAndCompile f inp = compile (populateExpressions f) inp
    where
        populateExpressions filt = case filt of
            Slice (e1, e2)                  -> Slice (populateExpressions e1, populateExpressions e2)
            ArrayIndexing e                 -> ArrayIndexing (populateExpressions e)
            ArrayIndexingOptional e         -> ArrayIndexingOptional (populateExpressions e)
            Pipe f1 f2                      -> Pipe (populateExpressions f1) (populateExpressions f2)
            Comma f1 f2                     -> Comma (populateExpressions f1) (populateExpressions f2)
            Parentheses f1                  -> Parentheses (populateExpressions f1)
            NestedExpression _ e            -> NestedExpression inp (populateExpressions e)
            ArrayConstructor e              -> ArrayConstructor (populateExpressions e)
            ObjectConstructor e             -> ObjectConstructor (populateExpressions e)
            IfThenElse a b c                -> IfThenElse (populateExpressions a) (populateExpressions b) (populateExpressions c)
            TryCatch a b                    -> TryCatch (populateExpressions a) (populateExpressions b)
            e                               -> e


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
