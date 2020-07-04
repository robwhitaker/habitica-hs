{-# LANGUAGE RecordWildCards #-}

module Web.Habitica.Types.WebhookMessage where

import           Data.Aeson                 (FromJSON, (.:))
import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)

import           Web.Habitica.Types.Helpers (DecoderNotImplemented (..))
import           Web.Habitica.Types.Task
import           Web.Habitica.Types.User    (Stats)

data WebhookMessage
    = TaskActivity TaskActivity
-- TODO:    | UserActivity
-- TODO:    | QuestActivity
    | GroupChatReceived GroupChatReceived
  deriving (Show, Eq, Ord)

instance FromJSON WebhookMessage where
    parseJSON = Aeson.withObject "WebhookMessage" $ \o -> do
        webhookType <- o .: "webhookType"
        case webhookType :: Text of
            "taskActivity" -> TaskActivity <$> Aeson.parseJSON (Aeson.Object o)
            "groupChatReceived" -> GroupChatReceived <$> Aeson.parseJSON (Aeson.Object o)
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

data MessageSender
    = SystemMessage
    | UserMessage UserMessageSender
  deriving (Eq, Show, Ord)

-- TODO: It may be better to eventually implement these in User instead of here
data Contributor
data Backer
data UserStyles

data UserMessageSender = UserMessageSender
    { umsId          :: UUID
    , umsUser        :: Text
    , umsUsername    :: Text
    , umsClient      :: Text
    , umsContributor :: DecoderNotImplemented Contributor
    , umsBacker      :: DecoderNotImplemented Backer
    , umsUserStyles  :: DecoderNotImplemented UserStyles
    } deriving (Show, Eq, Ord)

instance FromJSON UserMessageSender where
    parseJSON = Aeson.withObject "UserMessageSender" $ \o ->
        UserMessageSender
            <$> o .: "id"
            <*> o .: "user"
            <*> o .: "username"
            <*> o .: "client"
            <*> o .: "contributor"
            <*> o .: "backer"
            <*> o .: "userStyles"

data MessageGroup = MessageGroup
    { groupId   :: UUID
    , groupName :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON MessageGroup where
    parseJSON = Aeson.withObject "MessageGroup" $ \o ->
        MessageGroup
            <$> o .: "id"
            <*> o .: "name"

-- TODO: "flags": {}???
data MessageFlags

-- TODO: "info": {}
--       "info": { "type": "spell_cast_party", "user": "☠️ pandemikorrr ☠️", "class": "healer", "spell": "healAll" }
--       system msg only?
data MessageInfo

data MessageChat = MessageChat
    { mcFlagCount       :: Int
    , mcFlags           :: DecoderNotImplemented MessageFlags
    , mcId              :: UUID
    , mcText            :: Text
    , mcUnformattedText :: Text
    , mcInfo            :: DecoderNotImplemented MessageInfo
    , mcTimestamp       :: UTCTime
    , mcSender          :: MessageSender -- actually the "uuid" field with more meta info on the user attached
    , mcGroupId         :: UUID
    } deriving (Show, Eq, Ord)

instance FromJSON MessageChat where
    parseJSON = Aeson.withObject "MessageChat" $ \o -> do
        mcFlagCount <- o .: "flagCount"
        mcFlags <- o .: "flags"
        mcId <- o .: "id"
        mcText <- o .: "text"
        mcUnformattedText <- o .: "unformattedText"
        mcInfo <- o .: "info"
        mcTimestamp <- o .: "timestamp"
        mcGroupId <- o .: "groupId"

        sender <- o .: "uuid"
        mcSender <-
            if (sender :: Text) == "system"
            then return SystemMessage
            else UserMessage <$> Aeson.parseJSON (Aeson.Object o)

        return MessageChat {..}

data GroupChatReceived = GCR
    { gcrGroup  :: MessageGroup
    , gcrChat   :: MessageChat
    , gcrUserId :: UUID
    } deriving (Show, Eq, Ord)

instance FromJSON GroupChatReceived where
    parseJSON = Aeson.withObject "GroupChatReceived" $ \o ->
        GCR
            <$> o .: "group"
            <*> o .: "chat"
            <*> (o .: "user" >>= (.: "_id"))
