{-# LANGUAGE DeriveGeneric #-}

module Feishu.Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON)

data App = App {app_id :: Text, app_secret :: Text} deriving (Show, Eq, Generic)


instance FromJSON App 
instance ToJSON App 