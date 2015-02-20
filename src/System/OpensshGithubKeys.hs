module System.OpensshGithubKeys (formatKey) where

import Data.List (intercalate)

import qualified Data.Text as T

import Network.Octohat.Types (Member(..), MemberWithKey(..), PublicKey(..))

-- | Returns a list of keys for the user, with the username appended for easier
-- identification.
formatKey :: MemberWithKey -> [String]
formatKey mkeys =
  map (\k -> intercalate " " [(T.unpack $ publicKey k), ghUsername])
  (memberKey mkeys)

  where ghUsername = (T.unpack . memberLogin . member) mkeys
