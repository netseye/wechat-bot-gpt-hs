{-# LANGUAGE OverloadedStrings #-}

module Wechat.Message where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Network.Wreq (defaults, header, postWith, responseBody)
import System.Environment (lookupEnv)

getWorkToolId :: IO (Maybe String)
getWorkToolId = lookupEnv "WECHAT_WORKTOOL_APP_ID"

sendMessage :: String -> String -> String -> IO ()
sendMessage name robotId msg = do
  let url = "https://worktool.asrtts.cn/wework/sendRawMessage?robotId=" ++ robotId
  putStrLn $ "url: " ++ url
  let opts =
        defaults
          & header "Content-Type" .~ ["application/json"]
  let payload =
        object
          [ "socketType" .= (2 :: Int),
            "list"
              .= [ object
                     [ "type" .= (203 :: Int),
                       "titleList" .= [name],
                       "receivedContent" .= msg
                     ]
                 ]
          ]
  r <- postWith opts url (toJSON payload)
  print $ r ^. responseBody
