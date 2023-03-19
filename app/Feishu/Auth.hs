-- data AuthToken = AuthToken {token :: String, expires :: UTCTime}
{-# LANGUAGE OverloadedStrings #-}

module Feishu.Auth where

import Control.AutoUpdate
  ( UpdateSettings (updateAction, updateFreq, updateSpawnThreshold),
    defaultUpdateSettings,
    mkAutoUpdate,
  )
import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

utcTimeToText :: UTCTime -> Text
utcTimeToText t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" t

getCurrentTimeString :: IO Text
getCurrentTimeString = do
  utcTimeToText <$> getCurrentTime

getToken :: IO Text
getToken =
  join $
    mkAutoUpdate
      defaultUpdateSettings
        { updateFreq = 1000 * 1000000,
          updateAction = getCurrentTimeString,
          updateSpawnThreshold = 1
        }
