{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative

import qualified Data.Text as T

import Network.Octohat (keysOfTeamInOrganization)
import Network.Octohat.Types (runGitHub, OrganizationName(..),
                              TeamName(..))

import System.OpensshGithubKeys (formatKey)

import qualified Configuration.Dotenv as Dotenv

data Options = Options
  { localUser    :: String
  , organization :: String
  , team         :: String
  , users        :: [String]
  , dotfile      :: Maybe FilePath

  } deriving (Show)


fetchKeys :: Options -> IO ()
fetchKeys opts = do
  if any (\u -> localUser opts == u) (users opts)
    then do
      res <- runGitHub $ keysOfTeamInOrganization
             (OrganizationName $ T.pack $ organization opts)
             (TeamName $ T.pack $ team opts)

      case res of
        Right membersWithKeys ->
          putStrLn $ unlines $ concatMap formatKey membersWithKeys

        Left e -> error $ "Error retrieving keys for organization '" ++ show e

    else return ()

config :: Parser Options
config = Options
     <$> argument str (metavar "AUTHENTICATE"
                       <> help "Local user trying to authenticate currently")

     <*> strOption (
             long "organization"
             <> short 'o'
             <> metavar "ORGANIZATION"
             <> help "GitHub organization from which to select teams")

     <*> strOption (
                  long "team"
                  <> short 't'
                  <> metavar "TEAM"
                  <> help "GitHub team from which to select members' keys" )

     <*> some (strOption (
                  long "user"
                  <> short 'u'
                  <> metavar "USER"
                  <> help "A local user that we should try to authenticate using Github" ))

     <*> strOptional (
                  long "dotfile"
                  <> short 'f'
                  <> metavar "DOTFILE"
                  <> help "File in 'dotfile' format specifying GITHUB_TOKEN" )

strOptional :: Mod OptionFields String -> Parser (Maybe String)
strOptional flags = Just <$> strOption flags <|> pure Nothing

main :: IO ()
main = do
  options <- execParser opts
  readDotenvFile options
  fetchKeys options

  where
    opts = info (helper <*> config)
      ( fullDesc

     <> progDesc (unlines
        [ "Fetches ssh public keys from TEAMS under ORGANIZATION on GitHub."
        , "The output format is suitable for AuthorizedKeysCommand in"
        , "sshd_config. Requires a github token, which can be specified"
        , "in an environment variable GITHUB_TOKEN or in a dotenv file which"
        , "can be specified with the -f option."])

     <> header "openssh-github-keys - fetches team member keys from GitHub"
      )

-- | Conditionally reads the ~/.github_token file if it exists.
readDotenvFile :: Options -> IO ()
readDotenvFile opts =
  case dotfile opts of
    Just f -> Dotenv.loadFile False f
    Nothing -> return ()
