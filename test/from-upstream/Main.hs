module Main (main) where

import Parsing.Parsing
import Jq.Main as Jq (process)

import Paths_JqClone

import Control.Concurrent.STM (atomically)
import System.Process.Typed ( setStdin
                            , setStdout
                            , byteStringInput
                            , byteStringOutput
                            , shell
                            , withProcessWait_
                            , getStdout
                            , waitExitCodeSTM)
import Control.DeepSeq (force)
import Control.Exception (handle, evaluate, SomeException, ErrorCall)
import Data.ByteString.Lazy.UTF8 ( fromString
                                 , toString)

import Data.Maybe (isJust, fromJust)

import System.Exit (ExitCode(..), exitFailure, exitSuccess)

import Test.Tasty
import Test.Tasty.Runners (consoleTestReporter)
import Test.Tasty.HUnit

data JTestInstance = JTestInstance {program :: String, input :: String, output :: [String]} deriving Show

data JTest = JTestOne JTestInstance
           | JTestGroup { groupName :: String
                        , groupTests :: [JTest]}
           deriving (Show)

comment :: Parser ()
comment = fmap (const ()) $ char '\n'
                         <|> do char '#'
                                many (sat (/='\n'))
                                char '\n'

test :: Parser JTestInstance
test = do
  p <- some (sat (/='\n'))
  char '\n'
  i <- some (sat (/='\n'))
  char '\n'
  o <- tillemptyline
  return $ JTestInstance p i o
  where
    tillemptyline = do
      h <- some (sat (/='\n'))
      (char '\n') <|> return '\n'
      t <- (tillemptyline <|> return [])
      return $ h : t

testsAndComments :: Parser [JTestInstance]
testsAndComments =
  fmap (map fromJust . filter isJust) . many $  (fmap (const Nothing) comment)
                                            <|> (fmap Just test)

testfile :: Parser JTest
testfile = do
  ts <- testsAndComments
  return $ JTestGroup "Input-output tests" (map JTestOne ts)


toparse :: IO String
toparse = do
  file <- getDataFileName "jq.test"
  r <- readFile file
  return r

alltests :: IO JTest
alltests = do
  t <- toparse
  case (parse testfile t) of
    [(ts, [])] -> return ts
    [] -> do putStrLn "couldn't parse tests"
             exitFailure
    [(_, tail)] -> do putStrLn "couldn't parse tests"
                      putStrLn "leftover: "
                      putStrLn (take 30 tail)
                      exitFailure

type Program = (String -> String -> IO (ExitCode,String))

getProgram :: Maybe String -> Program
getProgram Nothing filter i = do
  r <- handle someh $ return $ force $ Jq.process [filter] i
  return $ case r of
    Left s -> (ExitFailure 2, s)
    Right out -> (ExitSuccess, out)
  where
    someh :: SomeException -> IO (Either String String)
    someh _ = return $ Left "caught some exception"

getProgram (Just s) filter i = do
  let sc = setStdin (byteStringInput (fromString i))
         $ setStdout byteStringOutput
         $ (shell $ s ++ " '" ++ filter ++ "'")
  (e, bv) <- withProcessWait_ sc (\p -> do o <- atomically (getStdout p)
                                           e <- atomically (waitExitCodeSTM p)
                                           return (e, o))
  return $ (e, toString bv)

prettifyJSON :: String -> IO String
prettifyJSON s = do
  let jqconfig = setStdin (byteStringInput (fromString s))
               $ setStdout byteStringOutput
               $ (shell "jq --sort-keys -M \".\"")
  bv <- withProcessWait_ jqconfig (\ p -> atomically (getStdout p))
  return $ toString bv
-- if you're on Windows and see problems with \r character consider
-- swapping the line above for
-- return . filter (/= '\r') . toString $ bv

createTestInstance :: Program -> JTestInstance -> TestTree
createTestInstance program t = localOption (mkTimeout 10000000) $ testCase name res
  where
    name = "filter: `" ++ p ++ "`\tinput: `" ++ i ++ "`"
    JTestInstance p i o = t
    res = do
      (exitcode, out) <- program p i
      assertEqual ("Program terminated abnormally with error: " ++ out)
                  ExitSuccess exitcode
      op <- fmap concat $ traverse prettifyJSON o
      assertEqual "The outputs don't match " op out

createTest :: Program -> JTest -> TestTree
createTest p (JTestOne j) =
  createTestInstance p j
createTest p (JTestGroup name gt) =
    testGroup name (map (createTest p) gt)

main :: IO ()
main = do
  -- to test the testsuite
  -- let program = getProgram (Just "jq --sort-keys -M")
  -- to test the submission
  let program = getProgram Nothing
  ts <- alltests
  defaultMainWithIngredients [consoleTestReporter] $
    createTest program ts
