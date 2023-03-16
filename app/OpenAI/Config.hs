{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Config where

import Control.Lens ((&), (.~))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wreq (Options, defaults, header)
import System.Environment (lookupEnv)
import Types.Configuration (Configuration (..), createEmptyConfiguration)

fromEnvVariables :: IO Configuration
fromEnvVariables = do
  apiKeyValue <- lookupEnv "OPENAI_API_KEY"
  orgValue <- lookupEnv "OPENAI_ORGANIZATION"
  case apiKeyValue of
    Nothing -> error "Error: Missing environment variable 'OPENAI_API_KEY'! This variable is required and needs to be set before proceeding. "
    Just apiKey ->
      pure $
        createEmptyConfiguration
          { apiKey = T.pack apiKey,
            organization = T.pack $ fromMaybe "" orgValue
          }

fromValues :: Text -> Text -> Configuration
fromValues apiKeyValue orgValue =
  createEmptyConfiguration
    { apiKey = apiKeyValue,
      organization = orgValue
    }

-- | getHeaders function takes in a 'Configuration' data and returns an 'Options' data
-- It sets the header "Content-Type" to "application/json"
-- It sets the header "Authorization" to "Bearer " and the apiKey of the passed configuration
-- It sets the header "OpenAI-Organization" to the organization of the passed configuration
getHeaders :: Configuration -> Options
getHeaders configuration =
  defaults
    & header "Content-Type" .~ ["application/json"]
    & header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 (apiKey configuration)]
    & header "OpenAI-Organization" .~ [TE.encodeUtf8 (organization configuration)]