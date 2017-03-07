{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config
  ( Config (..) )
where

import Data.Aeson
import Data.Text (Text)

-- | The application's configuration.

data Config = Config
  { configOrganization :: Text -- ^ GitHub Organization name
  , configTeam         :: Text -- ^ Name of team in that organization
  , configCacheFile    :: FilePath -- ^ Where to keep the cache
  } deriving (Eq, Ord, Show)

instance FromJSON Config where
  parseJSON = withObject "OpenSSH GitHub key configuration" $ \o -> do
    configOrganization <- o .: "organization"
    configTeam         <- o .: "team"
    configCacheFile    <- o .: "cache_file"
    return Config {..}
