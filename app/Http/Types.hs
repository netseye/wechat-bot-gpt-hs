{-# LANGUAGE DeriveGeneric #-}

module Http.Types where

import GHC.Generics (Generic)
import Data.Swagger (ToSchema)
import Data.Aeson (ToJSON,FromJSON)

data Resp a = Resp
  { message :: String,
    code :: Int,
    respData :: a
  }
  deriving (Generic, Show)

data Message = Message
  { messageType :: String,
    info :: Info
  }
  deriving (Generic, Show)

newtype Info = Info
  { text :: String
  }
  deriving (Generic, Show)


instance FromJSON a => FromJSON (Resp a)

instance ToJSON a => ToJSON (Resp a)

instance ToSchema a => ToSchema (Resp a)

instance FromJSON Message

instance ToJSON Message

instance ToSchema Message

instance FromJSON Info

instance ToJSON Info

instance ToSchema Info


data BotReq  = BotReq {
  spoken :: String,
  rawSpoken :: String,
  receivedName :: String,
  groupName :: String,
  groupRemark :: String,
  roomType :: Int,
  atMe :: Bool
} deriving (Generic, Show)


instance FromJSON BotReq

instance ToJSON BotReq

instance ToSchema BotReq