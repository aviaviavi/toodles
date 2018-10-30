{-# LANGUAGE DeriveDataTypeable,
             DataKinds,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeOperators #-}

module Config where

import           Paths_toodles

import           Data.Text              (Text)
import           Data.Version           (showVersion)
import           System.Console.CmdArgs

data ToodlesArgs = ToodlesArgs
  { directory       :: FilePath
  , assignee_search :: Maybe SearchFilter
  , limit_results   :: Int
  , port            :: Maybe Int
  , no_server       :: Bool
  } deriving (Show, Data, Typeable, Eq)

newtype SearchFilter =
  AssigneeFilter AssigneeFilterRegex
  deriving (Show, Data, Eq)

newtype AssigneeFilterRegex = AssigneeFilterRegex Text
                                  deriving (Show, Data, Eq)

argParser :: ToodlesArgs
argParser = ToodlesArgs
          { directory = def &= typFile &= help "Root directory of your project"
          , assignee_search = def &= help "Filter todo's by assignee"
          , limit_results = def &= help "Limit number of search results"
          , port = def &= help "Run server on port"
          , no_server = def &= help "Output matching todos to the command line and exit"
          } &= summary ("toodles " ++ showVersion version)
            &= program "toodles"
            &= verbosity
            &= help "Manage TODO's directly from your codebase"
