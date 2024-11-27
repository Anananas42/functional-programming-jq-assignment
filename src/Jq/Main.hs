module Jq.Main (main, process) where

import Jq.Filters (filters)
import Jq.Parser
import Jq.Compiler

import System.Environment (getArgs)

import Control.Arrow (left)

readInput :: IO String
readInput = getContents

process :: [String] -> String -> Either String String
process args json = do
  v <- parseConfig args
  obj <- maybe (Left "Couldn't parse JSON") Right $ parse parseJSON json
  let program = populateExpressionsAndCompile . filters $ v
  res <- left ("Couldn't execute the program: " ++) $ run program obj
  return $ concatMap ((++"\n") . show) res

processIO :: [String] -> String -> IO (Either String ())
processIO c s = do
  case process c s of
    Left e -> return $ Left e
    Right v -> do putStr v; return $ Right ()

getInputs :: IO ([String], String)
getInputs = do
  args <- getArgs
  toparse <- readInput
  return (args, toparse)

main :: IO ()
main = do
  (args, inp) <- getInputs
  v <- processIO args inp
  case v of
    Left s -> putStrLn s
    _ -> return ()
