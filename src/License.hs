{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module License
  (readLicense) where

import           Paths_toodles

import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8  as LB
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           System.Directory
import           System.Process

data UserTier
  = BadLicense
  | NoLiscense
  | Individual
  | Commercial
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data License = License {
  payload          :: ToodlesLicense,
  encoded          :: Text,
  payloadSignature :: Text
                       } deriving (Generic, FromJSON, ToJSON, Show)

data ToodlesLicense = ToodlesLicense
  { validStart :: Integer
  , validEnd   :: Integer
  , email      :: Text
  , reference  :: Text
  , product    :: Text
  } deriving (Generic, FromJSON, ToJSON, Show)

readLicense :: FilePath -> FilePath  -> IO (Either String UserTier)
readLicense publicKeyPath licensePath  = do
  licenseExists <- doesFileExist licensePath
  putStrLn licensePath
  if not licenseExists then
    return $ Right NoLiscense
  else do
    parsedContents <- eitherDecodeFileStrict licensePath
    either (return . Left) (isLicenseValid publicKeyPath) parsedContents

isLicenseValid :: FilePath -> License -> IO (Either String UserTier)
isLicenseValid publicKeyPath (License _ encodedPayload sig) = do
  dataDir <- getDataDir
  now <- ((* 1000) . round) `fmap` getPOSIXTime
  let args =
        [ dataDir ++ "/verify.py"
        , publicKeyPath
        , T.unpack sig
        , T.unpack encodedPayload
        ]
      decodedPayload =
        decode (B64.decodeLenient . LB.pack $ T.unpack encodedPayload)
  result <-
    catch
      (readProcess "python" args "")
      (\(e :: IOException) ->
         return $ displayException e)
  return $
    let validated = ("True" == (T.strip $ T.pack result))
    in if validated && (maybe 0 validEnd decodedPayload >= now)
        then Right Commercial
        else Left "Invalid license file"
