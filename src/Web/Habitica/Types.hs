module Web.Habitica.Types
    ( module Web.Habitica.Types.Task
    , module Web.Habitica.Types.User
    , DecoderNotImplemented (..)
    , EmptyObject (..)
    ) where

import           Data.Aeson              (FromJSON (..))
import qualified Data.Aeson              as Aeson

import           Web.Habitica.Types.Task
import           Web.Habitica.Types.User

-- Misc types

data EmptyObject
    = EmptyObject
  deriving (Show, Eq, Ord)

instance FromJSON EmptyObject where
    parseJSON = Aeson.withObject "EmptyObject" $ \_ -> return EmptyObject

-- PLACEHOLDERS

data DecoderNotImplemented a =
    DecoderNotImplemented

instance FromJSON (DecoderNotImplemented a) where
    parseJSON _ = return DecoderNotImplemented
