{-# LANGUAGE DeriveAnyClass,
             DeriveGeneric #-}

module Types where

import Data.Aeson             (ToJSON, FromJSON)
import Data.IORef             (IORef)
import Data.Text              (Text)
import GHC.Generics           (Generic)

data SourceFile = SourceFile
  { fullPath    :: FilePath
  , sourceLines :: [Text]
  } deriving (Show)

type LineNumber = Integer

data ToodlesState = ToodlesState
  { results  :: IORef TodoListResult,
    dataPath :: FilePath
  }

data TodoEntry
  = TodoEntryHead { entryId          :: Integer
                  , body             :: [Text]
                  , assignee         :: Maybe Text
                  , sourceFile       :: FilePath
                  , lineNumber       :: LineNumber
                  , priority         :: Maybe Integer
                  , customAttributes :: [(Text, Text)]
                  , tags             :: [Text]
                  , leadingText      :: Text }
  | TodoBodyLine Text
  deriving (Show, Generic, ToJSON)

data TodoListResult = TodoListResult
  { todos   :: [TodoEntry]
  , message :: Text
  } deriving (Show, Generic, ToJSON)

newtype DeleteTodoRequest = DeleteTodoRequest
  { ids :: [Integer]
  } deriving (Show, Generic, FromJSON)

data EditTodoRequest = EditTodoRequest
  { editIds     :: [Integer]
  , setAssignee :: Maybe Text
  , addTags     :: [Text]
  , addKeyVals  :: [(Text, Text)]
  , setPriority :: Maybe Integer
  } deriving (Show, Generic, FromJSON)
