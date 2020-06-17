{-# LANGUAGE RecordWildCards #-}

module Web.Habitica.Types where

import           Data.Char             (toLower)
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.UUID             (UUID)
import           Data.Void             (Void)

import           Data.Aeson            (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson            as Aeson
import           Data.Aeson.Types      (Pair)

-- TASK --

data TaskChecklistItem = TaskChecklistItem
    { ciCompleted :: Bool
    , ciText      :: Text
    , ciId        :: UUID
    , ciLinkId    :: Maybe Text -- type is text in Habitica code, but not sure if it should really be UUID maybe?
    } deriving (Show, Eq, Ord)

instance FromJSON TaskChecklistItem where
    parseJSON = Aeson.withObject "TaskChecklistItem" $ \o ->
        TaskChecklistItem
            <$> o .: "completed"
            <*> o .: "text"
            <*> o .: "id"
            <*> o .:? "linkId"

instance ToJSON TaskChecklistItem where
    toJSON TaskChecklistItem {..} =
        Aeson.object
            [ "completed" .= ciCompleted
            , "text" .= ciText
            , "id" .= ciId
            , "linkId" .= ciLinkId
            ]

data TaskHistoryItem = TaskHistoryItem
    { historyValue      :: Double
    , historyDate       :: POSIXTime
    , historyScoredUp   :: Maybe Int -- only exists for habits
    , historyScoredDown :: Maybe Int -- only exists for habits
    } deriving (Show, Eq, Ord)

instance FromJSON TaskHistoryItem where
    parseJSON = Aeson.withObject "TaskHistoryItem" $ \o ->
        TaskHistoryItem
            <$> o .: "value"
            <*> o .: "date"
            <*> o .:? "scoredUp"
            <*> o .:? "scoredDown"

instance ToJSON TaskHistoryItem where
    toJSON TaskHistoryItem {..} =
        Aeson.object
            [ "value" .= historyValue
            , "date" .= historyDate
            , "scoredUp" .= historyScoredUp
            , "scoredDown" .= historyScoredDown
            ]

data HabitFrequency
    = HDaily
    | HWeekly
    | HMonthly
  deriving (Show, Eq, Ord)

instance FromJSON HabitFrequency where
    parseJSON = Aeson.withText "HabitFrequency" $ \case
        "daily" -> return HDaily
        "weekly" -> return HWeekly
        "monthly" -> return HMonthly
        _ -> fail "HabitFrequency must be one of: daily, weekly, or monthly"

instance ToJSON HabitFrequency where
    toJSON = Aeson.toJSON . fmap toLower . drop 1 . show

data DailyFrequency
    = DDaily
    | DWeekly
    | DMonthly
    | DYearly
  deriving (Show, Eq, Ord)

instance FromJSON DailyFrequency where
    parseJSON = Aeson.withText "HabitFrequency" $ \case
        "daily" -> return DDaily
        "weekly" -> return DWeekly
        "monthly" -> return DMonthly
        "yearly" -> return DYearly
        _ -> fail "DailyFrequency must be one of: daily, weekly, monthly, or yearly"

instance ToJSON DailyFrequency where
    toJSON = Aeson.toJSON . fmap toLower . drop 1 . show

data DailyRepeat = DailyRepeat
    { repeatMonday    :: Bool
    , repeatTuesday   :: Bool
    , repeatWednesday :: Bool
    , repeatThursday  :: Bool
    , repeatFriday    :: Bool
    , repeatSaturday  :: Bool
    , repeatSunday    :: Bool
    } deriving (Show, Eq, Ord)

instance FromJSON DailyRepeat where
    parseJSON = Aeson.withObject "DailyRepeat" $ \o ->
        DailyRepeat
            <$> o .: "m"
            <*> o .: "t"
            <*> o .: "w"
            <*> o .: "th"
            <*> o .: "f"
            <*> o .: "s"
            <*> o .: "su"

instance ToJSON DailyRepeat where
    toJSON DailyRepeat {..} =
        Aeson.object
            [ "m" .= repeatMonday
            , "t" .= repeatTuesday
            , "w" .= repeatWednesday
            , "th" .= repeatThursday
            , "f" .= repeatFriday
            , "s" .= repeatSaturday
            , "su" .= repeatSunday
            ]

data TaskHabit = TaskHabit
    { habitUp          :: Bool
    , habitDown        :: Bool
    , habitCounterUp   :: Int
    , habitCounterDown :: Int
    , habitFrequency   :: HabitFrequency
    , habitHistory     :: [TaskHistoryItem]
    } deriving (Show, Eq, Ord)

habitToPairs :: TaskHabit -> [Pair]
habitToPairs TaskHabit {..} =
    [ "up" .= habitUp
    , "down" .= habitDown
    , "counterUp" .= habitCounterUp
    , "counterDown" .= habitCounterDown
    , "frequency" .= habitFrequency
    , "history" .= habitHistory
    ]

instance FromJSON TaskHabit where
    parseJSON = Aeson.withObject "TaskHabit" $ \o ->
        TaskHabit
            <$> o .: "up"
            <*> o .: "down"
            <*> o .: "counterUp"
            <*> o .: "counterDown"
            <*> o .: "frequency"
            <*> o .: "history"

instance ToJSON TaskHabit where
    toJSON = Aeson.object . habitToPairs

data TaskDaily = TaskDaily
    { dailyCompleted         :: Bool
    , dailyCollapseChecklist :: Bool
    , dailyChecklist         :: [TaskChecklistItem]
    , dailyFrequency         :: DailyFrequency
    , dailyEveryX            :: Int
    , dailyStartDate         :: UTCTime
    , dailyRepeat            :: DailyRepeat
    , dailyStreak            :: Int
    , dailyDaysOfMonth       :: [Int]
    , dailyWeeksOfMonth      :: [Int]
    , dailyIsDue             :: Bool
    , dailyNextDue           :: [UTCTime]
    , dailyYesterDaily       :: Bool
    , dailyHistory           :: [TaskHistoryItem]
    } deriving (Show, Eq, Ord)

dailyToPairs :: TaskDaily -> [Pair]
dailyToPairs TaskDaily {..} =
    [ "completed" .= dailyCompleted
    , "collapseChecklist" .= dailyCollapseChecklist
    , "checklist" .= dailyChecklist
    , "frequency" .= dailyFrequency
    , "everyX" .= dailyEveryX
    , "startDate" .= dailyStartDate
    , "repeat" .= dailyRepeat
    , "streak" .= dailyStreak
    , "daysOfMonth" .= dailyDaysOfMonth
    , "weeksOfMonth" .= dailyWeeksOfMonth
    , "isDue" .= dailyIsDue
    , "nextDue" .= dailyNextDue
    , "yesterDaily" .= dailyYesterDaily
    , "history" .= dailyHistory
    ]

instance FromJSON TaskDaily where
    parseJSON = Aeson.withObject "TaskDaily" $ \o ->
        TaskDaily
            <$> o .: "completed"
            <*> o .: "collapseChecklist"
            <*> o .: "checklist"
            <*> o .: "frequency"
            <*> o .: "everyX"
            <*> o .: "startDate"
            <*> o .: "repeat"
            <*> o .: "streak"
            <*> o .: "daysOfMonth"
            <*> o .: "weeksOfMonth"
            <*> o .: "isDue"
            <*> o .: "nextDue"
            <*> o .: "yesterDaily"
            <*> o .: "history"

instance ToJSON TaskDaily where
    toJSON = Aeson.object . dailyToPairs


data TaskTodo = TaskTodo
    { todoCompleted         :: Bool
    , todoCollapseChecklist :: Bool
    , todoChecklist         :: [TaskChecklistItem]
    , todoDateCompleted     :: Maybe UTCTime
    , todoDate              :: Maybe Text -- Should be date, but:
                             -- https://github.com/HabitRPG/habitica/blob/develop/website/server/models/task.js#L365
    } deriving (Show, Eq, Ord)

todoToPairs :: TaskTodo -> [Pair]
todoToPairs TaskTodo {..} =
    [ "completed" .= todoCompleted
    , "collapseChecklist" .= todoCollapseChecklist
    , "checklist" .= todoChecklist
    , "dateCompleted" .= todoDateCompleted
    , "date" .= todoDate
    ]

instance FromJSON TaskTodo where
    parseJSON = Aeson.withObject "TaskTodo" $ \o ->
        TaskTodo
            <$> o .: "completed"
            <*> o .: "collapseChecklist"
            <*> o .: "checklist"
            <*> o .:? "dateCompleted"
            <*> o .:? "date"

instance ToJSON TaskTodo where
    toJSON = Aeson.object . todoToPairs

data Priority
    = Trivial
    | Easy
    | Medium
    | Hard
  deriving (Show, Eq, Ord)

instance FromJSON Priority where
    parseJSON = Aeson.withScientific "Priority" $ \rawPriority ->
        case fractionalToPriority rawPriority of
            Nothing -> fail "Priority must be one of: 0.1, 1, 1.5, or 2"
            Just priority -> return priority

instance ToJSON Priority where
    toJSON = Aeson.toJSON . (priorityToFractional :: Priority -> Float)

priorityToFractional :: Fractional a => Priority -> a
priorityToFractional = \case
    Trivial -> 0.1
    Easy -> 1
    Medium -> 1.5
    Hard -> 2

fractionalToPriority :: (Eq a, Fractional a) => a -> Maybe Priority
fractionalToPriority = \case
    0.1 -> Just Trivial
    1 -> Just Easy
    1.5 -> Just Medium
    2 -> Just Hard
    _ -> Nothing

data Attribute
    = Strength
    | Constitution
    | Intelligence
    | Perception
  deriving (Show, Eq, Ord)

instance FromJSON Attribute where
    parseJSON = Aeson.withText "Attribute" $ \case
        "str" -> return Strength
        "con" -> return Constitution
        "int" -> return Intelligence
        "per" -> return Perception
        _ -> fail "Attribute must be one of: str, con, int, or per"

instance ToJSON Attribute where
    toJSON attr = Aeson.toJSON (take 3 $ fmap toLower (show attr))

data BrokenChallengeType
    = CChallengeDeleted
    | CTaskDeleted
    | CUnsubscribed
    | CChallengeClosed
    | CChallengeTaskNotFound
  deriving (Show, Eq, Ord)

instance FromJSON BrokenChallengeType where
    parseJSON = Aeson.withText "BrokenChallengeType" $ \case
        "CHALLENGE_DELETED" -> return CChallengeDeleted
        "TASK_DELETED" -> return CTaskDeleted
        "UNSUBSCRIBED" -> return CUnsubscribed
        "CHALLENGE_CLOSED" -> return CChallengeClosed
        "CHALLENGE_TASK_NOT_FOUND" -> return CChallengeTaskNotFound
        _ -> fail "BrokenChallengeType must be one of CHALLENGE_DELETED, TASK_DELETED, UNSUBSCRIBED, CHALLENGE_CLOSED, or CHALLENGE_TASK_NOT_FOUND"

instance ToJSON BrokenChallengeType where
    toJSON = \case
        CChallengeDeleted -> "CHALLENGE_DELETED"
        CTaskDeleted -> "TASK_DELETED"
        CUnsubscribed -> "UNSUBSCRIBED"
        CChallengeClosed -> "CHALLENGE_CLOSED"
        CChallengeTaskNotFound -> "CHALLENGE_TASK_NOT_FOUND"

data TaskChallenge = TaskChallenge
    { tcShortName :: Maybe Text
    , tcId        :: Maybe UUID
    , tcTaskId    :: Maybe UUID
    , tcBroken    :: Maybe BrokenChallengeType
    , tcWinner    :: Maybe Text
    } deriving (Show, Eq, Ord)

instance FromJSON TaskChallenge where
    parseJSON = Aeson.withObject "TaskChallenge" $ \o ->
        TaskChallenge
            <$> o .:? "shortName"
            <*> o .:? "id"
            <*> o .:? "taskId"
            <*> o .:? "broken"
            <*> o .:? "winner"

instance ToJSON TaskChallenge where
    toJSON TaskChallenge {..} =
        Aeson.object
            [ "shortName" .= tcShortName
            , "id" .= tcId
            , "taskId" .= tcTaskId
            , "broken" .= tcBroken
            , "winner" .= tcWinner
            ]

data BrokenGroupType
    = GGroupDeleted
    | GTaskDeleted
    | GUnsubscribed
  deriving (Show, Eq, Ord)

instance FromJSON BrokenGroupType where
    parseJSON = Aeson.withText "BrokenChallengeType" $ \case
        "GROUP_DELETED" -> return GGroupDeleted
        "TASK_DELETED" -> return GTaskDeleted
        "UNSUBSCRIBED" -> return GUnsubscribed
        _ -> fail "BrokenGroupType must be one of GROUP_DELETED, TASK_DELETED, or UNSUBSCRIBED"

instance ToJSON BrokenGroupType where
    toJSON = \case
        GGroupDeleted -> "GROUP_DELETED"
        GTaskDeleted -> "TASK_DELETED"
        GUnsubscribed -> "UNSUBSCRIBED"

data TaskGroupApproval = TaskGroupApproval
    { tgaRequired      :: Bool
    , tgaApproved      :: Bool
    , tgaDateApproved  :: Maybe UTCTime
    , tgaApprovingUser :: Maybe UUID
    , tgaRequested     :: Bool
    , tgaRequestedDate :: Maybe UTCTime
    } deriving (Show, Eq, Ord)

instance FromJSON TaskGroupApproval where
    parseJSON = Aeson.withObject "TaskGroupApproval" $ \o ->
        TaskGroupApproval
            <$> o .: "required"
            <*> o .: "approved"
            <*> o .:? "dateApproved"
            <*> o .:? "approvingUser"
            <*> o .: "requested"
            <*> o .:? "requestedDate"

instance ToJSON TaskGroupApproval where
    toJSON TaskGroupApproval {..} =
        Aeson.object
            [ "required" .= tgaRequired
            , "approved" .= tgaApproved
            , "dateApproved" .= tgaDateApproved
            , "approvingUser" .= tgaApprovingUser
            , "requested" .= tgaRequested
            , "requestedDate" .= tgaRequestedDate
            ]

data TaskGroup = TaskGroup
    { tgId               :: Maybe UUID
    , tgBroken           :: Maybe BrokenGroupType
    , tgAssignedUsers    :: [UUID]
    , tgAssignedDate     :: Maybe UTCTime
    , tgTaskId           :: Maybe UUID
    , tgApproval         :: TaskGroupApproval
    , tgSharedCompletion :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON TaskGroup where
    parseJSON = Aeson.withObject "TaskGroup" $ \o ->
        TaskGroup
            <$> o .:? "id"
            <*> o .:? "broken"
            <*> o .: "assignedUsers"
            <*> o .:? "assignedDate"
            <*> o .:? "taskId"
            <*> o .: "approval"
            <*> o .: "sharedCompletion"

instance ToJSON TaskGroup where
    toJSON TaskGroup {..} =
        Aeson.object
            [ "id" .= tgId
            , "broken" .= tgBroken
            , "assignedUsers" .= tgAssignedUsers
            , "assignedDate" .= tgAssignedDate
            , "taskId" .= tgTaskId
            , "approval" .= tgApproval
            , "sharedCompletion" .= tgSharedCompletion
            ]

data Reminder = Reminder
    { reminderId        :: UUID
    , reminderStartDate :: Maybe UTCTime
    , reminderTime      :: UTCTime
    } deriving (Show, Eq, Ord)

instance FromJSON Reminder where
    parseJSON = Aeson.withObject "Reminder" $ \o ->
        Reminder
            <$> o .: "id"
            <*> o .:? "startDate"
            <*> o .: "time"

instance ToJSON Reminder where
    toJSON Reminder {..} =
        Aeson.object
            [ "id" .= reminderId
            , "startDate" .= reminderStartDate
            , "time" .= reminderTime
            ]

-- Does not have its own From/To-JSON since it needs to encode/decode
-- from the same structure as Task, as that's how Habitica expects the
-- data to be
data TaskType
    = Habit TaskHabit
    | Daily TaskDaily
    | Todo TaskTodo
    | Reward
  deriving (Show, Eq, Ord)

data Task = Task
    { taskId         :: UUID -- Needs to be encoded to both _id and id
    , taskType       :: TaskType
    , taskText       :: Text
    , taskNotes      :: Text
    , taskAlias      :: Maybe Text
    , taskTags       :: [UUID]
    , taskValue      :: Double
    , taskPriority   :: Priority
    , taskAttribute  :: Attribute
    , taskUserId     :: Maybe UUID -- When not set, it belongs to a challenge
    , taskChallenge  :: TaskChallenge
    , taskGroup      :: TaskGroup
    , taskReminders  :: [Reminder]
    , taskByHabitica :: Bool
    , taskCreatedAt  :: UTCTime
    , taskUpdatedAt  :: UTCTime
    } deriving (Show, Eq, Ord)

instance FromJSON Task where
    parseJSON = Aeson.withObject "Task" $ \o -> do
        taskId <- o .: "_id"
        typeOfTask <- o .: "type"
        taskType <- case typeOfTask :: Text of
            "habit" -> Habit <$> Aeson.parseJSON (Aeson.Object o)
            "daily" -> Daily <$> Aeson.parseJSON (Aeson.Object o)
            "todo" -> Todo <$> Aeson.parseJSON (Aeson.Object o)
            "reward" -> return Reward
            _ -> fail "TaskType must be one of: habit, daily, todo, or reward"
        taskText <- o .: "text"
        taskNotes <- o .: "notes"
        taskAlias <- o .:? "alias"
        taskTags <- o .: "tags"
        taskValue <- o .: "value"
        taskPriority <- o .: "priority"
        taskAttribute <- o .: "attribute"
        taskUserId <- o .:? "userId"
        taskChallenge <- o .: "challenge"
        taskGroup <- o .: "group"
        taskReminders <- o .: "reminders"
        taskByHabitica <- o .: "byHabitica"
        taskCreatedAt <- o .: "createdAt"
        taskUpdatedAt <- o .: "updatedAt"
        return Task {..}

instance ToJSON Task where
    toJSON Task {..} =
        let
            uniqueFields =
                case taskType of
                    Habit habit -> habitToPairs habit
                    Daily daily -> dailyToPairs daily
                    Todo todo   -> todoToPairs todo
                    Reward      -> []
        in
        Aeson.object $
            [ "_id" .= taskId
            , "id" .= taskId
            , "type" .=
                case taskType of
                  Habit _ -> "habit" :: Text
                  Daily _ -> "daily"
                  Todo _  -> "todo"
                  Reward  -> "reward"
            , "text" .= taskText
            , "notes" .= taskNotes
            , "alias" .= taskAlias
            , "tags" .= taskTags
            , "value" .= taskValue
            , "priority" .= taskPriority
            , "attribute" .= taskAttribute
            , "userId" .= taskUserId
            , "challenge" .= taskChallenge
            , "group" .= taskGroup
            , "reminders" .= taskReminders
            , "byHabitica" .= taskByHabitica
            , "createdAt" .= taskCreatedAt
            , "updatedAt" .= taskUpdatedAt
            ] <> uniqueFields

-- USER --

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

type EmptyObject = Maybe Void

-- PLACEHOLDERS

data DecoderNotImplemented a =
    DecoderNotImplemented

instance FromJSON (DecoderNotImplemented a) where
    parseJSON _ = return DecoderNotImplemented

data User
