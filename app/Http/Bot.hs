{-# LANGUAGE OverloadedStrings #-}

module Http.Bot where

import Http.Types
import Control.Monad.IO.Class

callBack :: (MonadIO m) => String -> BotReq -> m (Resp Message)
callBack name msg = do
  liftIO $ print name >> print msg
  pure Resp
    { message = "success",
      code = 0,
      respData =
        Message
          { messageType = "bot",
            info =
              Info
                { text = "body"
                }
          }
    }
