{-# LANGUAGE DeriveGeneric #-}

module Types.Resp where

import Data.Aeson
import GHC.Generics (Generic)

data ChatCompletion = ChatCompletion
  { id :: String,
    object :: String,
    created :: Integer,
    model :: String,
    usage :: Usage,
    choices :: [Choice]
  }
  deriving (Show, Generic)

data Usage = Usage
  { prompt_tokens :: Int,
    completion_tokens :: Int,
    total_tokens :: Int
  }
  deriving (Show, Generic)

data Choice = Choice
  { message :: Message
  -- finish_reason :: String,
  -- index :: Int
  }
  deriving (Show, Generic)

data Message = Message
  { role :: String,
    content :: String
  }
  deriving (Show, Generic)

instance ToJSON Message

instance ToJSON ChatCompletion

instance ToJSON Usage

instance ToJSON Choice

instance FromJSON Message

instance FromJSON ChatCompletion

instance FromJSON Usage

instance FromJSON Choice