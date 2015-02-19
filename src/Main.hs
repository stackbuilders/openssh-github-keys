{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative

import Data.List (intercalate)

import qualified Data.Text as T

import Network.Octohat (keysOfTeamInOrganization)
import Network.Octohat.Types (runGitHub, Member(..), MemberWithKey(..),
                              PublicKey(..))

import qualified Configuration.Dotenv as Dotenv

import System.Directory (doesFileExist, getHomeDirectory)
import Control.Monad (when)
import System.FilePath (combine)

data Options = Options
  { organization :: String
  , team         :: String
  } deriving (Show)


fetchKeys :: Options -> IO ()
fetchKeys opts = do
  res <- runGitHub $ keysOfTeamInOrganization
         (T.pack $ organization opts) (T.pack $ team opts)

  case res of
    Right membersWithKeys ->
      putStrLn $ unlines $ concatMap formatKey membersWithKeys

    Left e -> error $ "Error retrieving keys for organization '" ++ show e

config :: Parser Options
config = Options
     <$> strOption (
             long "organization"
             <> short 'o'
             <> metavar "ORGANIZATION"
             <> help "GitHub organization from which to select teams")

     <*> strOption (
                  long "team"
                  <> short 't'
                  <> metavar "TEAM"
                  <> help "GitHub team from which to select members' keys" )

main :: IO ()
main = do
  readDotenvFile

  execParser opts >>= fetchKeys
  where
    opts = info (helper <*> config)
      ( fullDesc

     <> progDesc (unlines
        [ "Fetches ssh public keys from TEAMS under ORGANIZATION on GitHub."
        , "The output format is suitable for AuthorizedKeysCommand in"
        , "sshd_config. Requires a github token, which can be specified"
        , "in a Dotenv file in ~/.github_token, or in an environment variable"
        , "GITHUB_TOKEN."])

     <> header "openssh-github-keys - fetches team member keys from GitHub"
      )

-- | Conditionally reads the ~/.github_token file if it exists.
readDotenvFile :: IO ()
readDotenvFile = do
  homePath <- getHomeDirectory
  let githubTokenFile = combine homePath ".github_token"

  dotenvFileExists <- doesFileExist githubTokenFile

  when dotenvFileExists $ Dotenv.loadFile False githubTokenFile


-- | Returns a list of keys for the user, with the username appended for easier
-- identification.
formatKey :: MemberWithKey -> [String]
formatKey mkeys =
  map (\k -> intercalate " " [(T.unpack $ publicKey k), ghUsername])
  (memberKey mkeys)

  where ghUsername = (T.unpack . memberLogin . member) mkeys
