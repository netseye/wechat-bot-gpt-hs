{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Client where

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens (_JSON)
import qualified Data.Text as T
import Network.Wreq
import OpenAI.Config
import OpenAI.Const
import qualified Types.Req as Req
import Types.Resp (ChatCompletion (..))

chatCompletion :: Req.Chat -> IO (Either String ChatCompletion)
chatCompletion chat = do
  config <- fromEnvVariables
  r <-
    postWith
      (getHeaders config)
      (T.unpack $ baseURL <> "chat/completions")
      (toJSON chat)
  case r ^? responseBody . _JSON of
    Just resp -> pure $ Right resp
    Nothing -> pure $ Left "Failed to decode response body"