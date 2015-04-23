module System.OpensshGithubKeys.ConfigSpec where

import           Options.Applicative             (ParserPrefs (..),
                                                  execParserPure,
                                                  getParseResult)
import           System.OpensshGithubKeys.Config (options, readFileOptions)
import           System.OpensshGithubKeys.Types  (FileOptions (..),
                                                  Options (..),
                                                  emptyFileOptions)
import           Test.Hspec

parserPrefs :: ParserPrefs
parserPrefs = ParserPrefs "" False False True 80

spec :: Spec
spec = do
  describe "options" $ do
    it "uses the default organization" $ do
      let defaults = FileOptions (Just "stackbuilders") Nothing Nothing
      let args = [ "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "overrides the default organization" $ do
      let defaults = FileOptions (Just "other-organization") Nothing Nothing
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "uses the default team" $ do
      let defaults = FileOptions Nothing (Just "team") Nothing
      let args = [ "--organization=stackbuilders"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "overrides the default team" $ do
      let defaults = FileOptions Nothing (Just "other-team") Nothing
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
      getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "uses the default users" $ do
       let defaults = FileOptions Nothing Nothing (Just ["user1", "user2"])
       let args = [ "--organization=stackbuilders"
                   , "--team=team"
                   , "--dotfile=/path/to/env"
                   , "user"
                   ]
       let expected = Options "user" "stackbuilders" "team" ["user1", "user2"] "/path/to/env"
       getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "overrides the default users" $ do
       let defaults = FileOptions Nothing Nothing (Just ["user1", "user2"])
       let args = [ "--organization=stackbuilders"
                   , "--team=team"
                   , "--user=user-to-add"
                   , "--dotfile=/path/to/env"
                   , "user"
                   ]
       let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
       getParseResult (execParserPure parserPrefs (options defaults) args) `shouldBe` Just expected

    it "uses the default dotfile" $ do
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/etc/openssh-github-keys/github.creds"
      getParseResult (execParserPure parserPrefs (options emptyFileOptions) args) `shouldBe` Just expected

    it "overrides the default dotfile" $ do
      let args = [ "--organization=stackbuilders"
                 , "--team=team"
                 , "--user=user-to-add"
                 , "--dotfile=/path/to/env"
                 , "user"
                 ]
      let expected = Options "user" "stackbuilders" "team" ["user-to-add"] "/path/to/env"
      getParseResult (execParserPure parserPrefs (options emptyFileOptions) args) `shouldBe` Just expected

  describe "readFileOptions" $ do
    it "returns an empty FileOptions if the file doesn't exists" $
      readFileOptions "/not/found" `shouldReturn` emptyFileOptions

    it "returns a FileOptions with the default values if file exists" $ do
      let expected = FileOptions (Just "stackbuilders")
                                 (Just "openssh-github-keys-team")
                                 (Just ["user1", "user2" ])
      readFileOptions "spec/openssh-github-keys.conf" `shouldReturn` expected
