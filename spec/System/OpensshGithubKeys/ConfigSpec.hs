module System.OpensshGithubKeys.ConfigSpec where

import           Options.Applicative             (ParserPrefs (..),
                                                  execParserPure,
                                                  getParseResult)
import           System.OpensshGithubKeys.Config (options, readDefaultValues)
import           System.OpensshGithubKeys.Types  (Options (..))
import           Test.Hspec

parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False True 80

opts :: Options
opts = Options "user" "stackbuilders" "team" ["user-to-add"] $ Just "/path/to/env"

spec :: Spec
spec = do
  describe "options" $ do
    it "uses the default organization" $ do
      let defaults = [("organization", "stackbuilders")]
      let args = [ "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

    it "overrides the default organization" $ do
      let defaults = [("organization", "other-organization")]
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

    it "uses the default team" $ do
      let defaults = [("team", "team")]
      let args = [ "--organization=stackbuilders"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

    it "overrides the default team" $ do
      let defaults = [("team", "other-team")]
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

    it "uses the default dotfile" $ do
      let defaults = [("dotfile", "/path/to/env")]
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

    it "overrides the default dotfile" $ do
      let defaults = [("dotfile", "/other/path/to/env")]
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just opts

  describe "readDefaultValues" $ do
    it "returns an empty list if the file doesn't exists" $
      readDefaultValues "/not/found" `shouldReturn` []

    it "returns a list with the default values if file exists" $
      readDefaultValues "spec/openssh-github-keys.conf" `shouldReturn` [ ("organization", "stackbuilders")
                                                                       , ("team", "openssh-github-keys-team")
                                                                       , ("dotfile", "/path/to/env")
                                                                       ]
