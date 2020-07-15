module Web.Habitica.Api where

import           Data.Aeson           (ToJSON)
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID

import           Network.HTTP.Req     (GET (..), MonadHttp, NoReqBody (..),
                                       POST (..), PUT (..), ReqBodyJson (..),
                                       (=:))

import           Web.Habitica.Request
import           Web.Habitica.Types

data TaskTypeFilter
    = OnlyHabits
    | OnlyDailies
    | OnlyTodos
    | OnlyRewards
    | OnlyCompletedTodos
  deriving (Show, Eq, Ord)

getTask :: (MonadHttp m, HabiticaApi m) => UUID -> m (HabiticaResponse Task)
getTask tId = do
    headers <- getAuthHeaders
    habiticaRequest GET ["tasks", UUID.toText tId] NoReqBody headers mempty

-- TODO: Due date is a possible filter but Habitica's docs don't describe the format
getUserTasks
    :: (MonadHttp m, HabiticaApi m)
    => Maybe TaskTypeFilter
    -> m (HabiticaResponse [Task])
getUserTasks mbTypeFilter = do
    headers <- getAuthHeaders
    habiticaRequest GET ["tasks", "user"] NoReqBody headers typeFilter
  where
    filterToText = \case
        OnlyHabits -> "habits" :: Text
        OnlyDailies -> "dailys"
        OnlyTodos -> "todos"
        OnlyRewards -> "rewards"
        OnlyCompletedTodos -> "completedTodos"
    typeFilter = maybe mempty (("type" =:) . filterToText) mbTypeFilter

updateTask :: (MonadHttp m, HabiticaApi m, ToJSON taskChanges) => UUID -> taskChanges -> m (HabiticaResponse Task)
updateTask tId taskUpdates = do
    headers <- getAuthHeaders
    habiticaRequest PUT ["tasks", UUID.toText tId] (ReqBodyJson taskUpdates) headers mempty

-- TODO: Add list of fields as an argument
getUser
    :: ( MonadHttp m
       , HabiticaApi m
       )
    => m (HabiticaResponse (PartialDecoder User))
getUser = do
    headers <- getAuthHeaders
    habiticaRequest GET ["user"] NoReqBody headers mempty

updateUser
    :: ( MonadHttp m
       , HabiticaApi m
       , ToJSON userChanges
       )
    => userChanges -> m (HabiticaResponse (PartialDecoder User))
updateUser userChanges = do
    headers <- getAuthHeaders
    habiticaRequest PUT ["user"] (ReqBodyJson userChanges) headers mempty

runCron :: (MonadHttp m, HabiticaApi m) => m (HabiticaResponse EmptyObject)
runCron = do
    headers <- getAuthHeaders
    habiticaRequest POST ["cron"] NoReqBody headers mempty

getMemberProfile :: (MonadHttp m, HabiticaApi m) => UUID -> m (HabiticaResponse (PartialDecoder Member))
getMemberProfile uuid = do
    headers <- getAuthHeaders
    habiticaRequest GET ["members", UUID.toText uuid] NoReqBody headers mempty
