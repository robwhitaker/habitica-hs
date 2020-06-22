module Web.Habitica.Types.Helpers where

import           Data.Aeson (FromJSON (..), Value)
import qualified Data.Aeson as Aeson

-- Misc types

data EmptyObject
    = EmptyObject
  deriving (Show, Eq, Ord)

instance FromJSON EmptyObject where
    parseJSON = Aeson.withObject "EmptyObject" $ \_ -> return EmptyObject

-- PLACEHOLDERS

newtype DecoderNotImplemented a =
    DecoderNotImplemented { rawJsonValue :: Value }
  deriving (Show, Eq)

instance FromJSON (DecoderNotImplemented a) where
    parseJSON = return . DecoderNotImplemented

instance Ord (DecoderNotImplemented a) where
    compare (DecoderNotImplemented x) (DecoderNotImplemented y) =
        compare (show x) (show y)

newtype PartialDecoder a =
    PartialDecoder { unPartialDecoder :: (a, Value) }

instance FromJSON a => FromJSON (PartialDecoder a) where
    parseJSON json =
        fmap PartialDecoder $
            (,) <$> parseJSON json <*> return json
