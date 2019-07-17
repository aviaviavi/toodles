{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           License
import           Paths_toodles
import           Server
import           Types

import           Control.Monad            (when)
import           Data.IORef               (newIORef)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T (unpack)
import           Network.Wai.Handler.Warp (run)
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           Text.Printf              (printf)

main :: IO ()
main = do
  dataDirLocal <- (return . takeDirectory) =<< getExecutablePath
  dataDirBuilt <- getDataDir
  useBinaryLocalDataDir <- doesDirectoryExist $ dataDirLocal <> "/web"
  useBuiltDataDir <- doesDirectoryExist $ dataDirBuilt <> "/web"
  when
    ((not useBinaryLocalDataDir) && (not useBuiltDataDir))
    (fail $
     "Couldn't initialize toodles, no valid data directory found. Please file a bug on Github. Directories tried: \n" ++
     dataDirLocal ++ "\n" ++ dataDirBuilt)
  let dataDir =
        if useBinaryLocalDataDir
          then dataDirLocal
          else dataDirBuilt
  licenseRead <-
    readLicense
      (dataDir ++ "/toodles-license-public-key.pem")
      "/etc/toodles/license.json"
  let license = (either (BadLicense) (id) licenseRead)
  userArgs <- toodlesArgs >>= setAbsolutePath
  case userArgs of
    (ToodlesArgs _ _ _ _ True _) -> do
      sResults <- runFullSearch userArgs
      mapM_ (putStrLn . prettyFormat) $ todos sResults
    _ -> do
      let webPort = fromMaybe 9001 $ port userArgs
      ref <- newIORef Nothing
      putStrLn $ "serving on " ++ show webPort
      tierRef <- newIORef license
      run webPort $ app $ ToodlesState ref (dataDir ++ "/web") tierRef

prettyFormat :: TodoEntry -> String
prettyFormat (TodoEntryHead _ l a p n entryPriority f _ _ _ _ _ _) =
  printf
    "Assignee: %s\n%s%s:%d\n%s - %s"
    (fromMaybe "None" a)
    (maybe "" (\x -> "Priority: " ++ show x ++ "\n") entryPriority)
    p
    n
    (show f)
    (unlines $ map T.unpack l)
prettyFormat a = error "Invalid type for prettyFormat: " ++ show a
