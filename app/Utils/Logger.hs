{-# LANGUAGE FlexibleContexts #-}

module Utils.Logger where

import Colog
  ( WithLog,
    cmap,
    fmtMessage,
    logDebug,
    logError,
    logInfo,
    logTextStdout,
    logWarning,
    usingLoggerT,
  )
import qualified Colog
import Data.Text (Text)

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq)

logger :: LogLevel -> Text -> IO ()
logger level msg = do
  let action = cmap fmtMessage logTextStdout
  usingLoggerT action (logText level msg)

logText :: WithLog env Colog.Message m => LogLevel -> Text -> m ()
logText level msg = do
  case level of
    Debug -> logDebug msg
    Info -> logInfo msg
    Warning -> logWarning msg
    Error -> logError msg