{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser
import Jq.Json

sepByCustom :: Parser a -> Parser b -> Parser [b]
sepByCustom sep pattern = do
                      firstElement <- pattern
                      restElements <- many (sep *> pattern)
                      return (firstElement : restElements)

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- char '.'
  return Identity

-- pipeHelper :: (String -> Filter) -> [String] -> Filter
-- pipeHelper filterCast xs = case xs of
--       [h] -> filterCast h
--       h:t -> Pipe (filterCast h) (pipeHelper filterCast t)
--       _   -> error ""

-- sequenceHelper :: (Filter -> Filter -> Filter) -> [Filter] -> Filter
-- sequenceHelper filterWithTwoInputs xs = case xs of
--       [h] -> h
--       h:t -> filterWithTwoInputs h (sequenceHelper filterWithTwoInputs t)
--       _   -> error "sequenceHelper does not accept empty lists or filters with #inputs /= 2"

parseStringIndexing :: Parser Filter -- .foo
parseStringIndexing = do
    _ <- token . char $ '.'
    index <- ident
    isOptional <- optional $ token . char $ '?'
    case isOptional of
      Just _  -> return $ StringIndexingOptional $ StringLiteral index
      Nothing -> return $ StringIndexing $ StringLiteral index

parseStringIndexingQuotes :: Parser Filter
parseStringIndexingQuotes = do
                              _ <- token . char $ '.'
                              _ <- char '"'
                              index <- concat <$> many (escapedChar <|> fmap (:[]) (noneOf "\""))
                              _ <- char '"'
                              return $ StringIndexing $ StringLiteral index

-- parseStringIndexingGeneric :: Parser Filter -- .["foo"] or .["foo", "bar"]
-- parseStringIndexingGeneric = do
--   _ <- token . char $ '.'
--   _ <- token . char $ '['
--   indexingExpression <- NestedExpression JPlaceholder <$> parseFilterWithCommasAndPipes
--   _ <- token . char $ ']'
--   isOptional <- optional $ token . char $ '?'
--   case isOptional of
--       Just _ -> return $ StringIndexingOptional indexingExpression
--       Nothing -> return $ StringIndexing indexingExpression


-- arraySliceHelper :: Parser Filter
-- arraySliceHelper = do
--                     start <- (Integer <$> integer) <|> (NestedExpression JPlaceholder <$> parseFilterWithCommasAndPipes) <|> return (Integer 0)
--                     _ <- token . char $ ':'
--                     end <- (Integer <$> integer) <|> (NestedExpression JPlaceholder <$> parseFilterWithCommasAndPipes) <|> return (Integer (maxBound :: Int))
--                     return $ ArraySlicing (start, end)


parseArraySlicingAndIndexing :: Parser Filter
parseArraySlicingAndIndexing = do
                      _ <- optional $ char '.'
                      _ <- char '['
                      _ <- space
                      indexingExpression <- NestedExpression JPlaceholder <$> (parseSlice <|> parseFilterWithCommasAndPipes)
                      _ <- space
                      _ <- char ']'
                      isOptional <- optional $ token . char $ '?'
                      case isOptional of
                        Just _  -> return $ ArrayIndexingOptional indexingExpression
                        Nothing -> return $ ArrayIndexing indexingExpression

parseValueIterator :: Parser Filter
parseValueIterator = do
                      _ <- optional $ token . char $ '.'
                      _ <- token . char $ '['
                      _ <- token . char $ ']'
                      return ValueIterator

continueParsingLeftover :: Parser Filter -> Parser Filter
continueParsingLeftover f = do
                              firstFilter <- f
                              maybeNextFilter <- optional parseStringAndArrayIndexingCombined
                              case maybeNextFilter of
                                Just nextFilter -> return $ Pipe firstFilter nextFilter
                                Nothing         -> return firstFilter


parseStringAndArrayIndexingCombined :: Parser Filter
parseStringAndArrayIndexingCombined = continueParsingLeftover (
                              parseStringIndexingQuotes
                          <|> parseValueIterator
                          <|> parseStringIndexing
                          <|> parseArraySlicingAndIndexing
                          )

parseParentheses :: Parser Filter
parseParentheses = do
  _ <- token . char $ '('
  f <- parseFilterWithCommasAndPipes
  _ <- token . char $ ')'
  return (Parentheses f)


parsePipe :: Parser Filter
parsePipe = do
              firstFilter <- parseFilter
              _ <- do (token . char $ '|') <|> (token . char $ ' ')
              secondFilter <- parseFilterWithCommasAndPipes
              return $ Pipe firstFilter secondFilter

parseComma :: Parser Filter
parseComma = do
                firstFilter <- parseKeyValue <|> parseFilterWithPipes
                _ <- token . char $ ','
                secondFilter <- parseFilterWithCommasAndPipes
                return $ Comma firstFilter secondFilter

parseConstructors :: Parser Filter
parseConstructors = (do
                      Number <$> parseFloat)
                <|> (do
                      _ <- string "false"
                      return $ Literal "false")
                <|> (do
                      _ <- string "true"
                      return $ Literal "true")
                <|> (do
                      _ <- string "null"
                      return $ Literal "null")
                <|> (do
                      _ <- string "[]"
                      return $ Literal "[]")
                <|> (do
                      _ <- string "{}"
                      return $ Literal "{}")
                <|> (do
                      _ <- token . char $ '['
                      expr <- parseFilterWithCommasAndPipes
                      _ <- token . char $ ']'
                      return $ ArrayConstructor expr)
                <|> (do
                      _ <- token . char $ '{'
                      expr <- parseFilterWithCommasAndPipes
                      _ <- token . char $ '}'
                      return $ ObjectConstructor expr)
                <|> parseStringLiteral

parseStringLiteral :: Parser Filter
parseStringLiteral = do
                      _ <- space
                      _ <- char '"'
                      str <- concat <$> many (escapedChar <|> fmap (:[]) (noneOf "\""))
                      _ <- char '"'
                      _ <- space
                      return $ StringLiteral str

parseBoolOp :: Parser Filter
parseBoolOp = (do
                    b1 <- parseFilter
                    _ <- token . string $ "=="
                    b2 <- parseFilter
                    return $ Eq b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . string $ "!="
                    b2 <- parseFilter
                    return $ Neq b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . string $ "<="
                    b2 <- parseFilter
                    return $ Lte b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . string $ ">="
                    b2 <- parseFilter
                    return $ Gte b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . char $ '<'
                    b2 <- parseFilter
                    return $ Lt b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . char $ '>'
                    b2 <- parseFilter
                    return $ Gt b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . string $ "and"
                    b2 <- parseFilter
                    return $ And b1 b2)
              <|> (do
                    b1 <- parseFilter
                    _ <- token . string $ "or"
                    b2 <- parseFilter
                    return $ Or b1 b2)
              <|> (do
                    _ <- token . string $ "not"
                    b1 <- parseFilter
                    return $ Not b1)

parseArithmetic :: Parser Filter
parseArithmetic = (do
                    v1 <- parseFilter
                    _ <- token . char $ '+'
                    v2 <- parseFilter
                    return $ Plus v1 v2)
              <|> (do
                    v1 <- parseFilter
                    _ <- token . char $ '-'
                    v2 <- parseFilter
                    return $ Minus v1 v2)
              <|> (do
                    v1 <- parseFilter
                    _ <- token . char $ '*'
                    v2 <- parseFilter
                    return $ Mult v1 v2)
              <|> (do
                    v1 <- parseFilter
                    _ <- token . char $ '/'
                    v2 <- parseFilter
                    return $ Div v1 v2)

parseSlice :: Parser Filter
parseSlice = do
              s <- parseFilterWithPipes <|> return (Number 0)
              _ <- token . char $ ':'
              e <- parseFilterWithPipes <|> return (Number $ fromIntegral (maxBound :: Int))
              return $ Slice (s, e)

parseKeyValue :: Parser Filter
parseKeyValue = do
                  key <- parseStringLiteral <|> parseFilterWithPipes
                  _ <- token . char $ ':'
                  value <- parseFilterWithPipes
                  return $ KeyValue key value

parseRecursiveDescent :: Parser Filter
parseRecursiveDescent = do
                          _ <- token . string $ ".."
                          return RecursiveDescent

parseIfThenElse :: Parser Filter
parseIfThenElse = do
                    _ <- symbol "if"
                    a <- NestedExpression JPlaceholder <$> parseFilterWithPipes
                    _ <- symbol "then"
                    b <- NestedExpression JPlaceholder <$> parseFilterWithPipes
                    isElse <- optional $ symbol "else"
                    c <- case isElse of
                                Just _  -> NestedExpression JPlaceholder <$> parseFilterWithPipes
                                Nothing -> return Identity
                    _ <- symbol "end"
                    return $ IfThenElse a b c

parseTryCatch :: Parser Filter
parseTryCatch = do
                  _ <- symbol "try"
                  a <- NestedExpression JPlaceholder <$> parseFilterWithPipes
                  _ <- symbol "catch"
                  b <- NestedExpression JPlaceholder <$> parseFilterWithPipes
                  return $ TryCatch a b

parseFilter :: Parser Filter
parseFilter = parseParentheses
          <|> parseConstructors
          <|> parseStringAndArrayIndexingCombined
          <|> parseRecursiveDescent
          <|> parseIdentity

parseFilterWithPipes :: Parser Filter
parseFilterWithPipes = parseParentheses
                   <|> parsePipe
                   <|> parseArithmetic
                   <|> parseBoolOp
                   <|> parseFilter

parseFilterWithCommasAndPipes :: Parser Filter
parseFilterWithCommasAndPipes = parseParentheses
                            <|> parseComma
                            <|> parseKeyValue
                            <|> parseIfThenElse
                            <|> parseTryCatch
                            <|> parseFilterWithPipes


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilterWithCommasAndPipes h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
