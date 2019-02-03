{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types where

import           License

import           Data.Aeson       (FromJSON, ToJSON, Value (String), parseJSON,
                                   toJSON)
import           Data.Aeson.Types (typeMismatch)
import           Data.Data
import           Data.IORef       (IORef)
import           Data.String      (IsString)
import           Data.Text        (Text)
import qualified Data.Text        as T (unpack)
import           GHC.Generics     (Generic)

data SourceFile = SourceFile
  { fullPath    :: FilePath
  , sourceLines :: [Text]
  } deriving (Show)

type LineNumber = Integer

data ToodlesState = ToodlesState
  { results  :: IORef (Maybe TodoListResult),
    dataPath :: FilePath,
    userTier :: IORef UserTier
  }

data CommentType = SingleLine | MultiLine deriving (Show, Eq, Generic)
instance ToJSON CommentType

data TodoEntry
  = TodoEntryHead { entryId          :: Integer
                  , body             :: [Text]
                  , assignee         :: Maybe Text
                  , sourceFile       :: FilePath
                  , lineNumber       :: LineNumber
                  , priority         :: Maybe Integer
                  , flag             :: Flag
                  , customAttributes :: [(Text, Text)]
                  , tags             :: [Text]
                  , leadingText      :: Text
                  , commentType      :: CommentType
                  , entryHeadOpened  :: Bool
                  , entryHeadClosed  :: Bool
                  }
  | TodoBodyLine
    Text -- the body
    Bool -- has opening tag
    Bool -- has closing tag
  | TodoBodyLineCombined
    [Text] -- the body
    Bool -- has opening tag
    Bool -- has closing tag
  deriving (Show, Generic)
instance ToJSON TodoEntry

data TodoListResult = TodoListResult
  { todos   :: [TodoEntry]
  , message :: Text
  } deriving (Show, Generic)
instance ToJSON TodoListResult

newtype DeleteTodoRequest = DeleteTodoRequest
  { ids :: [Integer]
  } deriving (Show, Generic)
instance FromJSON DeleteTodoRequest

data EditTodoRequest = EditTodoRequest
  { editIds     :: [Integer]
  , setAssignee :: Maybe Text
  , addTags     :: [Text]
  , addKeyVals  :: [(Text, Text)]
  , setPriority :: Maybe Integer
  } deriving (Show, Generic)
instance FromJSON EditTodoRequest

data GetLicenseResponse = GetLicenseResponse
  { toodlesTier:: UserTier
  } deriving (Show, Generic)
instance ToJSON GetLicenseResponse

data Flag = TODO | FIXME | XXX | UF UserFlag
  deriving (Generic)

newtype UserFlag = UserFlag Text
  deriving (Show, Eq, IsString, Data, Generic)

instance Show Flag where
  show TODO              = "TODO"
  show FIXME             = "FIXME"
  show XXX               = "XXX"
  show (UF (UserFlag x)) = T.unpack x

instance ToJSON Flag where
  toJSON TODO              = Data.Aeson.String "TODO"
  toJSON FIXME             = Data.Aeson.String "FIXME"
  toJSON XXX               = Data.Aeson.String "XXX"
  toJSON (UF (UserFlag x)) = Data.Aeson.String x

instance FromJSON Flag where
  parseJSON (Data.Aeson.String x) =
    case x of
      "TODO"  -> pure TODO
      "FIXME" -> pure FIXME
      "XXX"   -> pure XXX
      _       -> pure $ UF $ UserFlag x
  parseJSON invalid               = typeMismatch "UserFlag" invalid

instance ToJSON UserFlag where
  toJSON (UserFlag x) = Data.Aeson.String x

instance FromJSON UserFlag where
  parseJSON (Data.Aeson.String x) = pure $ UserFlag x
  parseJSON invalid               = typeMismatch "UserFlag" invalid
