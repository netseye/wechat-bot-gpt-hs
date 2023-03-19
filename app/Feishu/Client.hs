{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Feishu.Client where

import Control.Lens ((^?))
import Data.Aeson.Lens (AsValue (_String), key, _Integer)
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    Value,
    object,
  )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Feishu.Config (getHeaders, getToken)
import Feishu.Const (baseURL, tableAppToken, tableId)
import Feishu.Types (App)
import Network.Wreq (Response, postWith, putWith, responseBody)
import Text.Printf (printf)

-- tenant_access_token
-- https://open.feishu.cn/document/ukTMukTMukTM/ukDNz4SO0MjL5QzM/auth-v3/auth/tenant_access_token_internal
-- auth/v3/tenant_access_token/internal
tenantAccessToken :: App -> IO (Maybe (Text, Integer))
tenantAccessToken app = do
  let url = printf "%s/auth/v3/tenant_access_token/internal" baseURL
  r <- postApi url (toJSON app)
  case r ^? responseBody . key "expire" . _Integer of
    Just expire -> do
      return $ fmap (,expire) $ r ^? responseBody . key "tenant_access_token" . _String
    Nothing -> return Nothing

-- https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/reference/bitable-v1/app-table-record/create
-- bitable/v1/apps/:app_token/tables/:table_id/records
bitableAddRecord :: Text -> Text -> IO (Maybe Text)
bitableAddRecord sender content = do
  let url = printf "%s/bitable/v1/apps/%s/tables/%s/records" baseURL tableAppToken tableId :: String
  r <- postApi url (object ["fields" .= object ["提问者" .= sender, "提问内容" .= content]])
  return $ r ^? responseBody . key "data" . key "record" . key "record_id" . _String

-- https://open.feishu.cn/document/uAjLw4CM/ukTMukTMukTM/reference/bitable-v1/app-table-record/update
-- bitable/v1/apps/:app_token/tables/:table_id/records/:record_id
bitableUpdateRecord :: Text -> Text -> IO (Maybe Text)
bitableUpdateRecord recordId content = do
  timestamp <- getPOSIXTime
  let url = printf "%s/bitable/v1/apps/%s/tables/%s/records/%s" baseURL tableAppToken tableId recordId :: String
  r <- putApi url (object ["fields" .= object ["回答内容" .= content, "回答时间" .= (floor timestamp * 1000 :: Integer)]])
  return $ r ^? responseBody . key "data" . key "record" . key "record_id" . _String

postApi :: String -> Value -> IO (Response ByteString)
postApi url payload = do
  t <- getToken
  postWith (getHeaders t) url payload

putApi :: String -> Value -> IO (Response ByteString)
putApi url payload = do
  t <- getToken
  putWith (getHeaders t) url payload
