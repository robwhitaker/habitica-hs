module Web.Habitica.Types.WebhookMessage where

import           Data.Text               (Text)
import           Data.UUID               (UUID)

import           Data.Aeson              (FromJSON, (.:))
import qualified Data.Aeson              as Aeson

import           Web.Habitica.Types.Task
import           Web.Habitica.Types.User (Stats)

{- HLINT ignore WebhookMessage -}
data WebhookMessage
    = TaskActivity TaskActivity
-- TODO:    | UserActivity
-- TODO:    | QuestActivity
-- TODO:    | GroupChatReceived
  deriving (Show, Eq, Ord)

instance FromJSON WebhookMessage where
    parseJSON = Aeson.withObject "WebhookMessage" $ \o -> do
        webhookType <- o .: "webhookType"
        case webhookType :: Text of
            "taskActivity" -> TaskActivity <$> Aeson.parseJSON (Aeson.Object o)
            -- TODO: implement other decoders
            _ -> fail "decoder not implemented"

{- HLINT ignore TaskActivity -}
data TaskActivity
    = TAScored TaskScoredMsg
-- TODO:    | TACreated TaskActivityMsg
-- TODO:    | TAUpdated TaskActivityMsg
-- TODO:    | TADeleted TaskActivityMsg
-- TODO:    | TAChecklistScored TaskChecklistScoredMsg
  deriving (Show, Eq, Ord)

instance FromJSON TaskActivity where
    parseJSON = Aeson.withObject "TaskActivity" $ \o -> do
        activityType <- o .: "type"
        case activityType :: Text of
            "scored" -> TAScored <$> Aeson.parseJSON (Aeson.Object o)
            -- TODO: implement other decoders
            _        -> fail "decoder not implemented"

data TaskScoredMsg = TaskScoredMsg
    { tsDirection :: ScoreDirection
    , tsDelta     :: Double
    , tsTask      :: Task
    , tsUser      :: TaskScoredMsgUser
    } deriving (Show, Eq, Ord)

instance FromJSON TaskScoredMsg where
    parseJSON = Aeson.withObject "TaskScoredMsg" $ \o ->
        TaskScoredMsg
            <$> o .: "direction"
            <*> o .: "delta"
            <*> o .: "task"
            <*> o .: "user"

data TaskScoredMsgUser = TaskScoredMsgUser
    { tsuStats :: Stats
    , tsuId    :: UUID
    -- TODO: _tmp field - specified drops, damage, etc. for task completion
    --       but haven't been able to find out the actual shape of these fields
    --       or what is/isn't a Maybe type. They might all be Maybes.
    } deriving (Show, Eq, Ord)

instance FromJSON TaskScoredMsgUser where
    parseJSON = Aeson.withObject "TaskScoredMsgUser" $ \o ->
        TaskScoredMsgUser
            <$> o .: "stats"
            <*> o .: "_id"
            -- TODO: _tmp field
