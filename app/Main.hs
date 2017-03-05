{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp (..))
import Options.Applicative
import System.Environment (getEnv)
import System.Exit
import System.OpensshGithubKeys (fetchTeamKeys)
import qualified Config                as C
import qualified Configuration.Dotenv  as D
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Yaml             as Yaml

----------------------------------------------------------------------------
-- Command line options

data Opts = Opts
  { optsSettingsFile :: FilePath
  , optsDotenvFile   :: Maybe FilePath
  , optsOrganization :: Maybe Text
  , optsTeam         :: Maybe Text
  , optsVersion      :: Bool
  }

parserInfo :: ParserInfo Opts
parserInfo = info (helper <*> optionParser)
  ( fullDesc <>
    progDesc desc <>
    header "openssh-github-keys — fetch your team's SSH keys from GitHub" )
  where
    desc = unlines
      [ "Fetches ssh public keys from TEAMS under ORGANIZATION on GitHub."
      , "The output format is suitable for AuthorizedKeysCommand in"
      , "sshd_config. Requires a GitHub token, which can be specified"
      , "in an environment variable GITHUB_TOKEN." ]

optionParser :: Parser Opts
optionParser = Opts
  <$> strOption
  (  long "settings"
  <> short 's'
  <> value "/etc/openssh-github-key/settings.yaml"
  <> showDefault
  <> help "YAML file with settings to use" )
  <*> option (Just <$> str)
  (  long "dotenv"
  <> short 'e'
  <> value Nothing
  <> help "Dotenv file from which to load environment variables (optional)" )
  <*> option (Just . T.pack <$> str)
  (  long "organization"
  <> short 'o'
  <> value Nothing
  <> help "Organization (overwrites the one in settings file)" )
  <*> option (Just . T.pack <$> str)
  (  long "team"
  <> short 't'
  <> value Nothing
  <> help "Team (overwrites the one in settings file)" )
  <*> switch
  ( long "version"
  <> short 'v'
  <> help "Show version of the program" )

----------------------------------------------------------------------------
-- Main

instance MonadHttp IO where
  handleHttpException = throwIO

main :: IO ()
main = do
  Opts {..} <- execParser parserInfo
  case optsDotenvFile of
    Nothing -> return ()
    Just path ->
      D.onMissingFile (D.loadFile True path) $
        putStrLn ("Could not load env variables from: \"" ++ path ++ "\"")
  gitHubToken <- B8.pack <$> getEnv "GITHUB_TOKEN"
  econfig <- Yaml.decodeFileEither optsSettingsFile
  case econfig of
    Left err -> do
      putStrLn (Yaml.prettyPrintParseException err)
      exitFailure
    Right C.Config {..} -> do
      let orgName  = fromMaybe configOrganization optsOrganization
          teamName = fromMaybe configTeam         optsTeam
      r <- try (fetchTeamKeys gitHubToken orgName teamName)
      case r of
        Left err -> do
          print (err :: SomeException)
          exitFailure
        Right keys -> do
          let formatKey (login, ks) =
                let f k = k <> " " <> T.encodeUtf8 login
                in f <$> ks
          (B8.putStrLn . B8.unlines . concat) (formatKey <$> keys)
          exitSuccess
