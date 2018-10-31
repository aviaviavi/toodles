{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Paths_toodles
import           Server
import           Types

import           Data.IORef               (newIORef)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T (unpack)
import           Network.Wai.Handler.Warp (run)
import           System.Console.CmdArgs   (cmdArgs)
import           Text.Printf              (printf)

main :: IO ()
main = do
  userArgs <- cmdArgs argParser >>= setAbsolutePath
  sResults <- runFullSearch userArgs
  case userArgs of
    (ToodlesArgs _ _ _ _ True _) -> mapM_ (putStrLn . prettyFormat) $ todos sResults
    _ -> do
          let webPort = fromMaybe 9001 $ port userArgs
          ref <- newIORef sResults
          dataDir <- (++ "/web") <$> getDataDir
          putStrLn $ "serving on " ++ show webPort
          run webPort $ app $ ToodlesState ref dataDir

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead _ l a p n entryPriority f _ _ _) =
  printf
    "Assignee: %s\n%s%s:%d\n%s"
    (fromMaybe "None" a)
    (maybe "" (\x -> "Priority: " ++ show x ++ "\n") entryPriority)
    p
    n
    (show f)
    (unlines $ map T.unpack l)
prettyFormat (TodoBodyLine _) = error "Invalid type for prettyFormat"
