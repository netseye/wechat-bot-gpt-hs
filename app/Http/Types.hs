{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Http.Types where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, Value, object)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Multipart
  ( FromMultipart (..),
    Mem,
    MultipartData,
    lookupInput,
  )
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
  toJSON :: ToJSON a => Resp a -> Value
  toJSON (Resp m c d) =
    object
      [ "message" .= m,
        "code" .= c,
        "data" .= d
      ]

instance ToSchema a => ToSchema (Resp a)

instance FromJSON Message

instance ToJSON Message where
  toJSON :: Message -> Value
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
  { spoken :: Text,
    rawSpoken :: Text,
    receivedName :: Text,
    groupName :: Text,
    groupRemark :: Text,
    roomType :: Text,
    atMe :: Text
  }
  deriving (Generic, Show)

instance FromJSON BotReq

instance ToJSON BotReq

instance ToSchema BotReq

instance ToForm BotReq

instance FromForm BotReq

instance FromMultipart Mem BotReq where
  fromMultipart :: MultipartData Mem -> Either String BotReq
  fromMultipart form =
    BotReq
      <$> lookupInput "spoken" form
      <*> lookupInput "rawSpoken" form
      <*> lookupInput "receivedName" form
      <*> lookupInput "groupName" form
      <*> lookupInput "groupRemark" form
      <*> lookupInput "roomType" form
      <*> lookupInput "atMe" form