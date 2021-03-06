{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Web.Habitica.Request
    ( HabiticaAuthHeaders
    , XClient
    , HabiticaResponse
    , xClient
    , habiticaHeaders
    , habiticaHeadersFromText
    , defaultHabiticaHttpConfig
    , hideAPIKeyInExceptions
    , habiticaRequest
    , HabiticaRequest
    , runHabiticaRequest
    , HabiticaApi (..)
    , HabiticaError (..)
    , responseBody
    , responseStatusCode
    , responseStatusMessage
    , responseHeader
    , responseCookieJar
    , responseRateLimit
    ) where

import qualified Control.Exception      as Exception
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import           Control.Monad.Trans    (lift)

import           Data.Aeson             (FromJSON, (.:))
import qualified Data.Aeson             as Aeson
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T (decodeUtf8, encodeUtf8)
import           Data.Time              (NominalDiffTime, UTCTime)
import qualified Data.Time              as Time
import qualified Data.Time.Format       as TimeFmt
import           Data.UUID              (UUID)
import qualified Data.UUID              as UUID

import           Text.Read              (readMaybe)

import qualified Network.HTTP.Client    as HttpClient
import           Network.HTTP.Req       ((/:))
import qualified Network.HTTP.Req       as Req

newtype HabiticaAuthHeaders =
    HabiticaAuthHeaders (Req.Option 'Req.Https)

newtype XClient =
    XClient (UUID, Text)

xClient :: UUID -> Text -> XClient
xClient maintainerId appName =
    XClient (maintainerId, appName)

habiticaHeaders :: UUID -> UUID -> XClient -> HabiticaAuthHeaders
habiticaHeaders userId apiKey (XClient (maintainerId, appName)) =
    HabiticaAuthHeaders $ mconcat
        [ Req.header "x-api-user" (UUID.toASCIIBytes userId)
        , Req.header "x-api-key" (UUID.toASCIIBytes apiKey)
        , Req.header "x-client" clientString
        ]
  where
    clientString = UUID.toASCIIBytes maintainerId <> "-" <> T.encodeUtf8 appName

habiticaHeadersFromText :: Text -> Text -> Text -> Maybe HabiticaAuthHeaders
habiticaHeadersFromText userId apiKey appName = do
    uid <- UUID.fromText userId
    key <- UUID.fromText apiKey
    return $ habiticaHeaders uid key (xClient uid appName)

data HabiticaRateLimit = HabiticaRateLimit
    { rateLimitLimit      :: Int
    , rateLimitRemaining  :: Int
    , rateLimitReset      :: UTCTime
    , rateLimitRetryAfter :: Maybe NominalDiffTime
    } deriving (Show, Eq, Ord)

data HabiticaError = HabiticaError
    { errorKey     :: Text
    , errorMessage :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON HabiticaError where
    parseJSON = Aeson.withObject "HabiticaError" $ \o ->
        HabiticaError
            <$> o .: "error"
            <*> o .: "message"

newtype HabiticaResBody a =
    HabiticaResBody { unHabiticaResBody :: Either HabiticaError a }
  deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (HabiticaResBody a) where
    parseJSON = Aeson.withObject "HabiticaResBody" $ \o -> do
        success <- o .: "success"
        HabiticaResBody <$>
            if success
            then Right <$> o .: "data"
            else Left <$> Aeson.parseJSON (Aeson.Object o)

type HabiticaResponse a = Req.JsonResponse (HabiticaResBody a)

defaultHabiticaHttpConfig :: Req.HttpConfig
defaultHabiticaHttpConfig =
    Req.defaultHttpConfig
        { Req.httpConfigCheckResponse = \_ _ _ -> Nothing
        }

-- TODO: Might still be possible to see the headers in the error field
--       of HttpExceptionRequest: InvalidHeader, InvalidRequestHeader
hideAPIKeyInExceptions :: Req.HttpException -> Req.HttpException
hideAPIKeyInExceptions = \case
    Req.VanillaHttpException (HttpClient.HttpExceptionRequest request err) ->
        let
            requestApiMasked = request {
                HttpClient.requestHeaders = fmap (\header@(headerName, _) ->
                    if headerName == "x-api-key"
                    then (headerName, "(hidden)")
                    else header
                ) (HttpClient.requestHeaders request)
            }
        in
        Req.VanillaHttpException (HttpClient.HttpExceptionRequest requestApiMasked err)

    otherException -> otherException

habiticaRequest
    :: ( FromJSON a
       , Req.MonadHttp m
       , Req.HttpMethod method
       , Req.HttpBody body
       , Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body)
       )
    => method
    -> [Text]
    -> body
    -> HabiticaAuthHeaders
    -> Req.Option 'Req.Https
    -> m (HabiticaResponse a)
habiticaRequest method endpoint body (HabiticaAuthHeaders headers) opts =
    Req.req method url body Req.jsonResponse (headers <> opts)
  where
    apiBaseUrl = Req.https "habitica.com" /: "api" /: "v3"
    url = foldl (/:) apiBaseUrl endpoint

responseBody :: FromJSON a => HabiticaResponse a -> Either HabiticaError a
responseBody =
    unHabiticaResBody . Req.responseBody

responseStatusCode :: FromJSON a => HabiticaResponse a -> Int
responseStatusCode =
    Req.responseStatusCode

responseStatusMessage :: FromJSON a => HabiticaResponse a -> ByteString
responseStatusMessage =
    Req.responseStatusMessage

responseHeader :: FromJSON a => HabiticaResponse a -> ByteString -> Maybe ByteString
responseHeader =
    Req.responseHeader

responseCookieJar :: FromJSON a => HabiticaResponse a -> HttpClient.CookieJar
responseCookieJar =
    Req.responseCookieJar

responseRateLimit :: FromJSON a => HabiticaResponse a -> Maybe HabiticaRateLimit
responseRateLimit res = do
    rateLimitLimit <- responseHeader res "X-RateLimit-Limit" >>= readBS
    rateLimitRemaining <- responseHeader res "X-RateLimit-Remaining" >>= readBS
    rateLimitReset <-
        responseHeader res "X-RateLimit-Reset" >>=
            -- Habitica sends this in the format output my JavaScript's
            -- `new Date()`, so it has to be parsed manually
            TimeFmt.parseTimeM True TimeFmt.defaultTimeLocale
                "%a %b %e %Y %H:%M:%S %Z%z"
            . takeWhile (/='(') . T.unpack . T.decodeUtf8
    let rateLimitRetryAfter = fmap Time.secondsToNominalDiffTime $
            responseHeader res "Retry-After" >>= readBS
    return HabiticaRateLimit {..}
  where
    readBS :: Read a => ByteString -> Maybe a
    readBS = readMaybe . T.unpack . T.decodeUtf8

newtype HabiticaRequest a = HabiticaRequest (ReaderT (Req.HttpConfig, HabiticaAuthHeaders) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Req.MonadHttp HabiticaRequest where
    handleHttpException =
        HabiticaRequest . lift . Exception.throwIO . hideAPIKeyInExceptions
    getHttpConfig =
        HabiticaRequest $ asks fst

instance HabiticaApi HabiticaRequest where
    getAuthHeaders =
        HabiticaRequest $ asks snd

runHabiticaRequest :: (MonadIO m) => HabiticaAuthHeaders -> Req.HttpConfig -> HabiticaRequest a -> m a
runHabiticaRequest headers config (HabiticaRequest m) =
    liftIO $ runReaderT m (config, headers)

class HabiticaApi m where
    getAuthHeaders :: m HabiticaAuthHeaders
