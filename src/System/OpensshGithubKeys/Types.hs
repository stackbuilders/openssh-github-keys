module System.OpensshGithubKeys.Types where

import           Data.Maybe (listToMaybe)

data FileOptions = FileOptions
  { fileOptionsOrganization :: Maybe String
  , fileOptionsTeam         :: Maybe String
  , fileOptionsUsers        :: Maybe [String]
  } deriving (Show, Eq)

data Options = Options
  { localUser    :: String
  , organization :: String
  , team         :: String
  , users        :: [String]
  , dotfile      :: String
  } deriving (Show, Eq)

mapToFileOptions :: [(String, [String])] -> FileOptions
mapToFileOptions m = FileOptions organizationFromMap teamFromMap usersFromMap
  where
    organizationFromMap :: Maybe String
    organizationFromMap = lookupSingleValue "organization" m

    teamFromMap :: Maybe String
    teamFromMap = lookupSingleValue "team" m

    usersFromMap :: Maybe [String]
    usersFromMap = lookup "users" m

lookupSingleValue :: String -> [(String, [String])] -> Maybe String
lookupSingleValue _ [] = Nothing
lookupSingleValue k v  = lookup k v >>= listToMaybe

emptyFileOptions :: FileOptions
emptyFileOptions = FileOptions Nothing Nothing Nothing
