module System.OpensshGithubKeys.Types where

data Options = Options
  { localUser    :: String
  , organization :: String
  , team         :: String
  , users        :: [String]
  , dotfile      :: Maybe FilePath
  } deriving (Show, Eq)
