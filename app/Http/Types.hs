{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Types where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, object)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm (..), ToForm (..))

data Resp a = Resp
  { message :: String,
    code :: Int,
    respData :: a
  }
  deriving (Generic, Show)

data Message = Message
  { messageType :: Int,
    info :: Info
  }
  deriving (Generic, Show)

newtype Info = Info
  { text :: String
  }
  deriving (Generic, Show)

instance FromJSON a => FromJSON (Resp a)

-- Resp中 respData 别名为 data
instance ToJSON a => ToJSON (Resp a) where
  toJSON (Resp m c d) =
    object
      [ "message" .= m,
        "code" .= c,
        "data" .= d
      ]

instance ToSchema a => ToSchema (Resp a)

instance FromJSON Message

instance ToJSON Message where
  toJSON (Message m i) =
    object
      [ "type" .= m,
        "info" .= i
      ]

instance ToSchema Message

instance FromJSON Info

instance ToJSON Info

instance ToSchema Info

data BotReq = BotReq
  { spoken :: String,
    rawSpoken :: String,
    receivedName :: String,
    groupName :: String,
    groupRemark :: String,
    roomType :: Int,
    atMe :: Bool
  }
  deriving (Generic, Show)

instance FromJSON BotReq

instance ToJSON BotReq

instance ToSchema BotReq

instance ToForm BotReq

instance FromForm BotReq
