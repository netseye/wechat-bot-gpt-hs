{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Client where

import Control.Lens ((^?))
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Lens (_JSON)
import qualified Data.Text as T
import Network.Wreq (postWith, responseBody)
import OpenAI.Config (fromEnvVariables, getHeaders)
import OpenAI.Const (baseURL)
import qualified Types.Req as Req
import Types.Resp (ChatCompletion (..))

chatCompletion :: Req.Chat -> IO (Maybe ChatCompletion)
chatCompletion chat = do
  config <- fromEnvVariables
  r <-
    postWith
      (getHeaders config)
      (T.unpack $ baseURL <> "chat/completions")
      (toJSON chat)
  return $ r ^? responseBody . _JSON