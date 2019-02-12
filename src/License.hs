{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module License
  (
    UserTier(..),
ToodlesLicense(..),

    readLicense
  ) where

import           Paths_toodles

import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8  as LB
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           System.Directory
import           System.Exit
import           System.Process

data UserTier
  = BadLicense String
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
  (exitcode, stdout, stderr) <- readProcessWithExitCode "python" args ""
  putStrLn stderr
  return $
    let validated = ("True" == T.strip (T.pack stdout))
    in if (exitcode == ExitSuccess) &&
          validated && (maybe 0 validEnd decodedPayload >= now)
         then Right Commercial
         else Left "Invalid license file"
