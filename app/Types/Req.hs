{-# LANGUAGE DeriveGeneric #-}

module Types.Req where

import Data.Aeson
import GHC.Generics (Generic)

data Chat = Chat
  { stream :: Bool,
    model :: String,
    messages :: [Message],
    temperature :: Double
  }
  deriving (Show, Generic)

data Message = Message
  { role :: String,
    content :: String
  }
  deriving (Show, Generic)

instance ToJSON Message

instance ToJSON Chat