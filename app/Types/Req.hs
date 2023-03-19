{-# LANGUAGE DeriveGeneric #-}

module Types.Req where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Chat = Chat
  { stream :: Bool,
    model :: String,
    messages :: [Message],
    temperature :: Double
  }
  deriving (Show, Generic)

defaultChat :: Text -> Chat
defaultChat con =
  Chat
    { stream = False,
      model = "gpt-3.5-turbo",
      messages = [Message {role = "user", content = con}],
      temperature = 0.7
    }

data Message = Message
  { role :: String,
    content :: Text
  }
  deriving (Show, Generic)

instance ToJSON Message

instance ToJSON Chat