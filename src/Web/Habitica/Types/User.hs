{-# LANGUAGE RecordWildCards #-}

module Web.Habitica.Types.User where

import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)

import           Data.Aeson                 (FromJSON, (.:), (.:?))
import qualified Data.Aeson                 as Aeson

import           Web.Habitica.Types.Helpers

-- TODO: fill in this placeholder
data User = User
    { userAuth    :: Maybe Auth
    , userParty   :: Maybe Party
    , userProfile :: Maybe Profile
    , userStats   :: Maybe Stats
    } deriving (Show, Eq, Ord)

instance FromJSON User where
    parseJSON = Aeson.withObject "User" $ \o -> do
        userAuth <- o .:? "auth"
        userParty <- o .:? "party"
        userProfile <- o .:? "profile"
        userStats <- o .:? "stats"
        return $ User {..}

-- AUTH FIELD --

data Auth = Auth
    { authBlocked    :: Maybe Bool
    , authLocal      :: AuthLocal
    , authTimestamps :: AuthTimestamps
    , authFacebook   :: DecoderNotImplemented AuthSocial
    , authGoogle     :: DecoderNotImplemented AuthSocial
    , authApple      :: DecoderNotImplemented AuthSocial
    } deriving (Show, Eq, Ord)

instance FromJSON Auth where
    parseJSON = Aeson.withObject "Auth" $ \o ->
        Auth
            <$> o .:? "blocked"
            <*> o .: "local"
            <*> o .: "timestamps"
            <*> o .: "facebook"
            <*> o .: "google"
            <*> o .: "apple"

data AuthLocal = AuthLocal
    { localEmail             :: Maybe Text -- TODO: maybe format as actual Email type?
    , localUsername          :: Text
    , localLowerCaseUsername :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON AuthLocal where
    parseJSON = Aeson.withObject "AuthLocal" $ \o ->
        AuthLocal
            <$> o .:? "email"
            <*> o .: "username"
            <*> o .: "lowerCaseUsername"

data AuthTimestamps = AuthTimestamps
    { atsCreated  :: UTCTime
    , atsLoggedIn :: UTCTime
    , atsUpdated  :: UTCTime
    } deriving (Show, Eq, Ord)

instance FromJSON AuthTimestamps where
    parseJSON = Aeson.withObject "AuthTimestamps" $ \o ->
        AuthTimestamps
            <$> o .: "created"
            <*> o .: "loggedin"
            <*> o .: "updated"

-- TODO: What is the shape of this thing?
data AuthSocial = AuthSocial
  deriving (Show, Eq, Ord)

-- PROFILE FIELD --

data Profile = Profile
    { profileBlurb    :: Maybe Text
    , profileImageUrl :: Maybe Text
    , profileName     :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON Profile where
    parseJSON = Aeson.withObject "Profile" $ \o ->
        Profile
            <$> o .:? "blurb"
            <*> o .:? "imageUrl"
            <*> o .: "name"

-- STATS FIELD --

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
    , statsToNextLevel :: Maybe Int
    , statsMaxHealth   :: Maybe Int
    , statsMaxMP       :: Maybe Int
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
            <*> o .:? "toNextLevel"
            <*> o .:? "maxHealth"
            <*> o .:? "maxMP"

-- Class --

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

-- PARTY FIELD --

data Party = Party
    { partyId             :: Maybe UUID -- Nothing if the user is not in a party
    , partyOrder          :: Text
    , partyOrderAscending :: Text
    , partyQuest          :: Quest
    } deriving (Show, Eq, Ord)

instance FromJSON Party where
    parseJSON = Aeson.withObject "Party" $ \o -> do
        partyId <- o .:? "_id"
        partyOrder <- o .: "order"
        partyOrderAscending <- o .: "orderAscending"
        partyQuest <- o .: "quest"
        return Party {..}

data Quest = Quest
    { questKey        :: Maybe Text -- Nothing if there is no current quest/quest invite
    , questProgress   :: QuestProgress
    , questCompleted  :: Maybe Text
        -- ^ From the Habitica source code:
        --     When quest is done, we move it from key => completed,
        --     and it's a one-time flag (for modal) that they unset by clicking "ok" in browser
    , questRsvpNeeded :: Bool
    } deriving (Show, Eq, Ord)

instance FromJSON Quest where
    parseJSON = Aeson.withObject "Quest" $ \o -> do
        questKey <- o .:? "key"
        questProgress <- o .: "progress"
        questCompleted <- o .:? "completed"
        questRsvpNeeded <- o .: "RSVPNeeded"
        return Quest {..}

data QuestProgress = QuestProgress
    { qpUp             :: Double -- Damage to boss
    , qpDown           :: Double -- No idea, maybe rage bar??
    , qpCollect        :: Maybe (Map Text Double) -- I think this is a map from collectible name to number
                                                  -- collected for a quest, but not sure
                                                  -- TODO: this maybe should be Int instead of Double?
    , qpCollectedItems :: Int -- TODO: best I can tell, this is always an Int but not 100% sure
    } deriving (Show, Eq, Ord)

instance FromJSON QuestProgress where
    parseJSON = Aeson.withObject "QuestProgress" $ \o -> do
        qpUp <- o .: "up"
        qpDown <- o .: "down"
        qpCollect <- o .:? "collect"
        qpCollectedItems <- o .: "collectedItems"
        return QuestProgress {..}
