{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Configuration.Dotenv            as Dotenv
import           Control.Monad                   (when)
import qualified Data.Text                       as T
import           Network.Octohat                 (keysOfTeamInOrganization)
import           Network.Octohat.Types           (OrganizationName (..),
                                                  TeamName (..), runGitHub)
import           Options.Applicative
import           System.OpensshGithubKeys        (formatKey)
import           System.OpensshGithubKeys.Config (options, readFileOptions)
import           System.OpensshGithubKeys.Types  (Options (..))
import           System.Timeout                  (timeout)

fetchKeys :: Options -> IO ()
fetchKeys os = when (any (\u -> localUser os == u) (users os)) $ do
  res <- runGitHub $ keysOfTeamInOrganization
         (OrganizationName $ T.pack $ organization os)
         (TeamName $ T.pack $ team os)

  case res of
   Right membersWithKeys -> putStrLn $ unlines $ concatMap formatKey membersWithKeys
   Left e -> error $ "Error retrieving keys for organization '" ++ show e

main :: IO ()
main = do
  d <- readFileOptions "/etc/openssh-github-keys/login.conf"
  opts <- execParser $ options d
  readDotenvFile opts
  res <- timeout allowedTime (fetchKeys opts)
  case res of
    Nothing -> error $ "Allowed time of " ++ show allowedTime ++
               " microseconds exceeded while fetching keys from GitHub."
    Just  _ -> return ()

  where
    allowedTime = 5 * 1000000 -- Allowed time in microseconds for fetching keys

-- | Conditionally reads the ~/.github_token file if it exists.
readDotenvFile :: Options -> IO ()
readDotenvFile = Dotenv.loadFile False . dotfile
