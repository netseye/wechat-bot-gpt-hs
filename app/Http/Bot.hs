{-# LANGUAGE OverloadedStrings #-}

module Http.Bot where

import Control.Monad.IO.Class (MonadIO (..))
import Http.Types
  ( BotReq,
    Info (Info, text),
    Message (..),
    Resp (..),
  )

callBack :: (MonadIO m) => String -> BotReq -> m (Resp Message)
callBack name msg = do
  liftIO $ print name >> print msg
  pure
    Resp
      { message = "success",
        code = 0,
        respData =
          Message
            { messageType = 5000,
              info =
                Info
                  { text = "body"
                  }
            }
      }
