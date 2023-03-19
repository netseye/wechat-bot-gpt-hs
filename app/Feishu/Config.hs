{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Feishu.Config where

import Control.Lens ((&), (.~), (^?))
import Control.Monad (void)
import Control.Monad.Cont (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Aeson.Lens (AsValue (_String), key, _Integer)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as TE
import Database.Redis
  ( Connection,
    connect,
    defaultConnectInfo,
    get,
    runRedis,
    setex,
  )
import Feishu.Const (baseURL)
import Feishu.Types (App (..))
import Network.Wreq (Options, defaults, header, postWith, responseBody)
import System.Environment (lookupEnv)
import Text.Printf (printf)

fromEnvVariables :: IO App
fromEnvVariables = do
  apiKeyValue <- lookupEnv "FEISHU_APP_ID"
  orgValue <- lookupEnv "FEISHU_APP_SECRET"
  case apiKeyValue of
    Nothing -> error "Error: Missing environment variable 'FEISHU_APP_ID'! This variable is required and needs to be set before proceeding. "
    Just k ->
      pure $
        App
          { app_id = T.pack k,
            app_secret = T.pack $ fromMaybe "" orgValue
          }

getToken :: IO Text
getToken = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    x <- get "authtoken"
    liftIO $ case x of
      Left _ -> return ""
      Right y -> do
        print y
        case y of
          Nothing -> do
            fromEnvVariables >>= tenantAccessToken >>= \t -> do
              case t of
                Nothing -> return ""
                Just z -> cachedToken conn z
          Just t -> return $ TE.decodeUtf8 t

getHeaders :: Text -> Options
getHeaders token =
  defaults
    & header "Content-Type" .~ ["application/json"]
    & header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 token]

cachedToken :: Connection -> (Text, Integer) -> IO Text
cachedToken conn y = do
  runRedis conn $ void $ setex "authtoken" (snd y - (3 * 60)) $ T.encodeUtf8 (fst y)
  return $ fst y

tenantAccessToken :: App -> IO (Maybe (Text, Integer))
tenantAccessToken app = do
  let url = printf "%s/auth/v3/tenant_access_token/internal" baseURL
  r <- postWith (getHeaders "") url (toJSON app)
  case r ^? responseBody . key "expire" . _Integer of
    Just expire -> do
      return $ fmap (,expire) $ r ^? responseBody . key "tenant_access_token" . _String
    Nothing -> return Nothing