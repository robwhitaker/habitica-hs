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

getTask :: (MonadHttp m, HabiticaApi m) => UUID -> m (HabiticaJsonResponse Task)
getTask tId = do
    headers <- getAuthHeaders
    habiticaRequest GET ["tasks", UUID.toText tId] NoReqBody headers mempty

-- TODO: Due date is a possible filter but Habitica's docs don't describe the format
getUserTasks
    :: (MonadHttp m, HabiticaApi m)
    => Maybe TaskTypeFilter
    -> m (HabiticaJsonResponse [Task])
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

updateTask :: (MonadHttp m, HabiticaApi m, ToJSON taskChanges) => UUID -> taskChanges -> m (HabiticaJsonResponse Task)
updateTask tId taskUpdates = do
    headers <- getAuthHeaders
    habiticaRequest PUT ["tasks", UUID.toText tId] (ReqBodyJson taskUpdates) headers mempty

-- TODO: this should return the updated user object, but we don't have decoders for that yet
updateUser
    :: ( MonadHttp m
       , HabiticaApi m
       , ToJSON userChanges
       )
    => userChanges -> m (HabiticaJsonResponse (DecoderNotImplemented User))
updateUser userChanges = do
    headers <- getAuthHeaders
    habiticaRequest PUT ["user"] (ReqBodyJson userChanges) headers mempty

runCron :: (MonadHttp m, HabiticaApi m) => m (HabiticaJsonResponse EmptyObject)
runCron = do
    headers <- getAuthHeaders
    habiticaRequest POST ["cron"] NoReqBody headers mempty
