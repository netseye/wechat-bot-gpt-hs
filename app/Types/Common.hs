{-# LANGUAGE DeriveGeneric #-}

module Types.Common where

import GHC.Generics (Generic)

data ChatCompletionRequestMessageRoleEnum = System | User | Assistant deriving (Show, Generic)

data ChatCompletionRequestMessage = ChatCompletionRequestMessage
  { role :: ChatCompletionRequestMessageRoleEnum,
    content :: String,
    name:: Maybe String
  } deriving (Show, Generic)