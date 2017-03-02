{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.OpensshGithubKeys
  ( fetchTeamKeys )
where

import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req
import Numeric.Natural
import qualified Data.ByteString.Char8 as B8

-- | Get pairs of user names and corresponding SSH keys for team members.

fetchTeamKeys :: MonadHttp m
  => ByteString        -- ^ GitHub authorization token
  -> Text              -- ^ GitHub organization name
  -> Text              -- ^ Team of interest in that organization
  -> m [(Text, [ByteString])]  -- ^ Username — SSH keys
fetchTeamKeys token orgName teamName' = do
  let gitHubApi    = https "api.github.com"
      listTeamsUrl = gitHubApi /: "orgs" /: orgName /: "teams"
      params =
        "page_size" =: (100 :: Int)               <>
        header "User-Agent" "openssh-github-keys" <>
        oAuth2Token token                         <>
        header "Accept" "application/vnd.github.v3.raw"
  teams <- req GET listTeamsUrl NoReqBody jsonResponse params
  let team = teamId <$> find
        ((== teamName') . teamName)
        (responseBody teams :: [Team])
  case team of
    Nothing -> return []
    Just teamId -> do
      let teamMembersUrl = gitHubApi /: "teams" /~ teamId /: "members"
      ms <- req GET teamMembersUrl NoReqBody jsonResponse params
      forM (memberLogin <$> responseBody ms) $ \login -> do
        keys <- req GET (https "github.com" /: (login <> ".keys"))
          NoReqBody bsResponse mempty
        return (login, B8.lines (responseBody keys))

----------------------------------------------------------------------------
-- Helpers

data Team = Team
  { teamId   :: Natural
  , teamName :: Text }

instance FromJSON Team where
  parseJSON = withObject "team in org" $ \o -> do
    teamId   <- o .: "id"
    teamName <- o .: "name"
    return Team {..}

data Member = Member
  { memberLogin :: Text }

instance FromJSON Member where
  parseJSON = withObject "team member" $ \o ->
    Member <$> (o .: "login")
