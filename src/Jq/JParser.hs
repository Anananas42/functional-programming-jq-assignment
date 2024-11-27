module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Control.Monad
import Data.Char
import Data.Maybe


parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

takeWhileParser :: (Char -> Bool) -> Parser String
takeWhileParser condition = P (\inp ->
    let (charToken, rest) = span condition inp
    in [(charToken, rest)])

noneOf :: [Char] -> Parser Char
noneOf cs = sat (`notElem` cs)

oneOf :: [Char] -> Parser Char
oneOf cs = sat (`elem` cs)

escapedChar :: Parser String
escapedChar = do
        _ <- char '\\'
        c <- item
        case c of
            'u'         -> parseUnicode
            nonUnicode  -> return ("\\" ++ [nonUnicode])

parseUnicode :: Parser String
parseUnicode = do
    digits <- replicateM 4 (sat isHexDigit)
    let hexValue = foldl (\acc x -> 16*acc + digitToInt x) 0 digits
    return [chr hexValue]

parseJString :: Parser JSON
parseJString = do _ <- char '"'
                  content <- concat <$> many (escapedChar <|> fmap (:[]) (noneOf "\"\\"))
                  _ <- char '"'
                  return (JString content)

parseFloat :: Parser Double
parseFloat = do
    sign <- optional (oneOf "+-")
    naturalPart <- optional $ some digit
    decimalPart <- optional (char '.' >> some digit)
    exponentPart <- optional (do
        _ <- oneOf "eE"
        expSign <- optional (oneOf "+-")
        digits <- some digit
        return $ maybe "" (:[]) expSign ++ digits)
    let signedPart = case sign of
                        Just '-' -> "-"
                        _        -> ""
    let numberStr = signedPart ++ fromMaybe "0" naturalPart ++ maybe "" ('.':) decimalPart ++ maybe "" ('E':) exponentPart
    guard $ isJust naturalPart || isJust decimalPart
    return $ read numberStr

parseJNumber :: Parser JSON
parseJNumber = JNumber <$> parseFloat

-- parseNumber :: Parser JSON
-- parseNumber = do
--                 num <- parseFloat
--                 return $ if num == fromIntegral (round num :: Integer) then JInt $ fromIntegral (round num :: Integer) else JNumber num

parseJBool :: Parser JSON
parseJBool = do
    string "true" >> return (JBool True)
    <|> (string "false" >> return (JBool False))

separateAndApply :: Parser a -> Parser sep -> Parser [a]
separateAndApply ch f = do
    x <- ch
    xs <- many (f >> ch)
    return (x:xs)

parseJObjectEntry :: Parser (String, JSON)
parseJObjectEntry = do
                        _ <- space
                        _ <- char '"'
                        key <- concat <$> many (escapedChar <|> fmap (:[]) (noneOf "\"\\"))
                        _ <- char '"'
                        _ <- space
                        _ <- char ':'
                        _ <- space
                        value <- parseJSON
                        return (key, value)

parseJObject :: Parser JSON
parseJObject = do
                _ <- space
                _ <- char '{'
                _ <- space
                content <- separateAndApply parseJObjectEntry (char ',') <|> return []
                _ <- space
                _ <- char '}'
                return (JObject content)

parseJArray :: Parser JSON
parseJArray = do
                _ <- space
                _ <- char '['
                _ <- space
                content <- separateAndApply parseJSON (char ',') <|> return []
                _ <- space
                _ <- char ']'
                return (JArray content)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull
                <|> parseJString
                <|> parseJNumber
                <|> parseJBool
                <|> parseJObject
                <|> parseJArray
