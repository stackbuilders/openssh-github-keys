{-# LANGUAGE OverloadedStrings #-}

module System.OpensshGithubKeysSpec where

import Network.Octohat.Types

import System.OpensshGithubKeys (formatKey)

import Test.Hspec (it, describe, shouldBe, Spec)

spec :: Spec
spec = do
  describe "formatKey" $ do
    it "formats the key including the github username" $ do
      let mkey =
            MemberWithKey { member = Member { memberLogin = "jsl"
                                            , memberId = 42 }

                          , memberKey = [
                               PublicKey { publicKeyId = 10
                                         , publicKey = "mykey" }
                               ]

                          , memberKeyFingerprint =
                               [ PublicKeyFingerprint {
                                    fingerprintId = 10
                                    , publicKeyFingerprint = "print" }
                               ]
                          }

      formatKey mkey `shouldBe` ["mykey jsl"]
