{-# LANGUAGE OverloadedStrings #-}

module Http.Bot where

import Control.Monad.IO.Class
import Http.Types

callBack :: (MonadIO m) => String -> BotReq -> m (Resp Message)
callBack name msg = do
  liftIO $ print name >> print msg
  pure
    Resp
      { message = "success",
        code = 0,
        respData =
          Message
            { messageType = 500,
              info =
                Info
                  { text = "body"
                  }
            }
      }
