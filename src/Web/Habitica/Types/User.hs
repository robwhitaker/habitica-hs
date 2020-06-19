module Web.Habitica.Types.User where

import           Data.Text               (Text)
import           Data.UUID               (UUID)

import           Data.Aeson              (FromJSON, (.:))
import qualified Data.Aeson              as Aeson

import           Web.Habitica.Types.Task

-- TODO: fill in this placeholder
data User = User

data Class
    = Warrior
    | Rogue
    | Wizard
    | Healer
  deriving (Show, Eq, Ord)

instance FromJSON Class where
    parseJSON = Aeson.withText "Class" $ \case
        "warrior" -> return Warrior
        "rogue" -> return Rogue
        "wizard" -> return Wizard
        "healer" -> return Healer
        _ -> fail "Class must be one of: warrior, rogue, wizard, or healer"

data Buffs = Buffs
    { buffsStr            :: Int
    , buffsInt            :: Int
    , buffsPer            :: Int
    , buffsCon            :: Int
    , buffsStealth        :: Int -- TODO: Not sure about this
    , buffsStreaks        :: Bool
    , buffsSnowball       :: Bool
    , buffsSpookySparkles :: Bool
    , buffsShinySeed      :: Bool
    , buffsSeafoam        :: Bool
    } deriving (Show, Eq, Ord)

instance FromJSON Buffs where
    parseJSON = Aeson.withObject "Buffs" $ \o ->
        Buffs
            <$> o .: "str"
            <*> o .: "int"
            <*> o .: "per"
            <*> o .: "con"
            <*> o .: "stealth"
            <*> o .: "streaks"
            <*> o .: "snowball"
            <*> o .: "spookySparkles"
            <*> o .: "shinySeed"
            <*> o .: "seafoam"

-- TODO: Tbh, I have no idea what this is, so Int might be the wrong type of number
data Training = Training
    { trainingInt :: Int
    , trainingPer :: Int
    , trainingStr :: Int
    , trainingCon :: Int
    } deriving (Show, Eq, Ord)

instance FromJSON Training where
    parseJSON = Aeson.withObject "Training" $ \o ->
        Training
            <$> o .: "int"
            <*> o .: "per"
            <*> o .: "str"
            <*> o .: "con"

data Stats = Stats
    { statsHp          :: Double
    , statsMp          :: Double
    , statsExp         :: Int
    , statsGp          :: Double
    , statsLvl         :: Int
    , statsClass       :: Class
    , statsPoints      :: Int
    , statsStr         :: Int
    , statsCon         :: Int
    , statsInt         :: Int
    , statsPer         :: Int
    , statsBuffs       :: Buffs
    , statsTraining    :: Training
    -- TODO: these last three might not always be defined as they are added by
    --       a function call and not part of the Stats schema, but I have yet
    --       to find a case when they're not. Might need to be fixed later.
    , statsToNextLevel :: Int
    , statsMaxHealth   :: Int
    , statsMaxMP       :: Int
    } deriving (Show, Eq, Ord)


instance FromJSON Stats where
    parseJSON = Aeson.withObject "Stats" $ \o ->
        Stats
            <$> o .: "hp"
            <*> o .: "mp"
            <*> o .: "exp"
            <*> o .: "gp"
            <*> o .: "lvl"
            <*> o .: "class"
            <*> o .: "points"
            <*> o .: "str"
            <*> o .: "con"
            <*> o .: "int"
            <*> o .: "per"
            <*> o .: "buffs"
            <*> o .: "training"
            <*> o .: "toNextLevel"
            <*> o .: "maxHealth"
            <*> o .: "maxMP"

-- WEBHOOK MESSAGES --

data ScoreDirection
    = ScoreUp
    | ScoreDown
  deriving (Show, Eq, Ord)

instance FromJSON ScoreDirection where
    parseJSON = Aeson.withText "ScoreDirection" $ \case
        "up" -> return ScoreUp
        "down" -> return ScoreDown
        _ -> fail "ScoreDirection must be one of: up or down"

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
