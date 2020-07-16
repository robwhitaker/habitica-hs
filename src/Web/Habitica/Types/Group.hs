{-# LANGUAGE RecordWildCards #-}

module Web.Habitica.Types.Group where

import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import           Data.UUID                  (UUID)

import           Data.Aeson                 (FromJSON, (.:), (.:?))
import qualified Data.Aeson                 as Aeson

import           Web.Habitica.Types.Helpers

data GroupType
    = GroupIsGuild
    | GroupIsParty
  deriving (Show, Eq, Ord)

instance FromJSON GroupType where
    parseJSON = Aeson.withText "GroupType" $ \case
        "guild" -> return GroupIsGuild
        "party" -> return GroupIsParty
        _ -> fail "GroupType must be one of: guild or party"

data GroupPrivacy
    = GroupIsPublic
    | GroupIsPrivate
  deriving (Show, Eq, Ord)

instance FromJSON GroupPrivacy where
    parseJSON = Aeson.withText "GroupPrivacy" $ \case
        "public" -> return GroupIsPublic
        "private" -> return GroupIsPrivate
        _ -> fail "GroupPrivacy must be one of: public or private"

-- TODO: needs implementation
data GroupTasksOrder
data GroupPurchased
data GroupManagers
data GroupQuestExtra

data Group = Group
    { groupName           :: Text
    , groupSummary        :: Maybe Text
    , groupDescription    :: Maybe Text
    , groupLeader         :: UUID
    , groupType           :: GroupType
    , groupPrivacy        :: GroupPrivacy
    -- Chat field left out since it's no longer in use
    , groupLeaderOnly     :: LeaderOnly
    , groupMemberCount    :: Int
    , groupChallengeCount :: Int
    , groupBalance        :: Double -- multiply this by 4 to get the number of gems the group has
    , groupLogo           :: Maybe Text
    , groupLeaderText     :: Maybe Text
    , groupQuest          :: GroupQuest
    , groupTasksOrder     :: DecoderNotImplemented GroupTasksOrder
    , groupPurchased      :: DecoderNotImplemented GroupPurchased
    , groupManagers       :: DecoderNotImplemented GroupManagers
    , groupCategories     :: [Category]
    } deriving (Show, Eq, Ord)

instance FromJSON Group where
    parseJSON = Aeson.withObject "Group" $ \o -> do
        groupName <- o .: "name"
        groupSummary <- o .:? "summary"
        groupDescription <- o .:? "description"
        groupLeader <- o .: "leader"
        groupType <- o .: "type"
        groupPrivacy <- o .: "privacy"
        groupLeaderOnly <- o .: "leaderOnly"
        groupMemberCount <- o .: "memberCount"
        groupChallengeCount <- o .: "challengeCount"
        groupBalance <- o .: "balance"
        groupLogo <- o .:? "logo"
        groupLeaderText <- o .:? "leaderText"
        groupQuest <- o .: "quest"
        groupTasksOrder <- o .: "tasksOrder"
        groupPurchased <- o .: "purchased"
        groupManagers <- o .: "managers"
        groupCategories <- o .: "categories"
        return Group {..}

data LeaderOnly = LeaderOnly
    { loChallenges :: Bool
    , loGetGems    :: Bool
    } deriving (Show, Eq, Ord)

instance FromJSON LeaderOnly where
    parseJSON = Aeson.withObject "LeaderOnly" $ \o -> do
        loChallenges <- o .: "challenges"
        loGetGems <- o .: "getGems"
        return LeaderOnly {..}

data Category = Category
    { categoryId   :: Text -- it's not a UUID... not sure if there is a better type

    -- I think these will always be defined, but the JS schema doesn't forbid Nothing
    -- so use Maybe's here to be safe
    , categorySlug :: Maybe Text
    , categoryName :: Maybe Text
    } deriving (Show, Eq, Ord)

instance FromJSON Category where
    parseJSON = Aeson.withObject "Category" $ \o -> do
        categoryId <- o .: "_id"
        categorySlug <- o .:? "slug"
        categoryName <- o .:? "name"
        return Category {..}

data GroupQuest = GroupQuest
    { gquestKey      :: Maybe Text
    , gquestActive   :: Bool
    , gquestLeader   :: Maybe UUID
    , gquestProgress :: GroupQuestProgress
    , gquestMembers  :: Map UUID Bool
    , gquestExtra    :: DecoderNotImplemented GroupQuestExtra -- TODO: no idea what this field even is
    } deriving (Show, Eq, Ord)

instance FromJSON GroupQuest where
    parseJSON = Aeson.withObject "GroupQuest" $ \o -> do
        gquestKey <- o .:? "key"
        gquestActive <- o .: "active"
        gquestLeader <- o .:? "leader"
        gquestProgress <- o .: "progress"
        gquestMembers <- o .: "members"
        gquestExtra <- o .: "extra"
        return GroupQuest {..}

data GroupQuestProgress = GroupQuestProgress
    { gqpHp      :: Maybe Double
    , gqpCollect :: Maybe (Map Text Int)
    , gqpRage    :: Maybe Double
    } deriving (Show, Eq, Ord)

instance FromJSON GroupQuestProgress where
    parseJSON = Aeson.withObject "GroupQuestProgress" $ \o -> do
        gqpHp <- o .:? "hp"
        gqpCollect <- o .:? "collect"
        gqpRage <- o .:? "rage"
        return GroupQuestProgress {..}
