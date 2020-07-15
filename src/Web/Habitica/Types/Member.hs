{-# LANGUAGE RecordWildCards #-}

module Web.Habitica.Types.Member where

import           Data.Text  (Text)
import           Data.Time  (UTCTime)

import           Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Aeson

-- TODO: rest of the fields
data Member = Member
    { memberAuth :: PublicAuth
    } deriving (Show, Eq, Ord)

instance FromJSON Member where
    parseJSON = Aeson.withObject "Member" $ \o -> do
        memberAuth <- o .: "auth"
        return $ Member {..}

-- MEMBER AUTH FIELD --

data PublicAuth = PublicAuth
    { pauthLocal      :: PublicAuthLocal
    , pauthTimestamps :: PublicAuthTimestamps
    } deriving (Show, Eq, Ord)

instance FromJSON PublicAuth where
    parseJSON = Aeson.withObject "PublicAuth" $ \o ->
        PublicAuth
            <$> o .: "local"
            <*> o .: "timestamps"

newtype PublicAuthLocal = PublicAuthLocal
    { palUsername          :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON PublicAuthLocal where
    parseJSON = Aeson.withObject "PublicAuthLocal" $ \o ->
        PublicAuthLocal <$> o .: "username"

data PublicAuthTimestamps = PublicAuthTimestamps
    { patsCreated  :: UTCTime
    , patsLoggedIn :: UTCTime
    , patsUpdated  :: UTCTime
    } deriving (Show, Eq, Ord)

instance FromJSON PublicAuthTimestamps where
    parseJSON = Aeson.withObject "PublicAuthTimestamps" $ \o ->
        PublicAuthTimestamps
            <$> o .: "created"
            <*> o .: "loggedin"
            <*> o .: "updated"
