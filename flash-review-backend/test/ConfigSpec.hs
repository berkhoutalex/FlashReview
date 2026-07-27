{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (spec) where

import           Config     (parseOrigins, resolvePort)
import           Test.Hspec

spec :: Spec
spec = do
  describe "resolvePort" $ do
    it "defaults to 8081 when PORT is unset" $
      resolvePort Nothing `shouldBe` 8081

    it "reads a numeric PORT" $
      resolvePort (Just "10000") `shouldBe` 10000

    it "falls back to the default when PORT is not a number" $
      resolvePort (Just "not-a-port") `shouldBe` 8081

  describe "parseOrigins" $ do
    it "defaults to the local dev origin when unset" $
      parseOrigins Nothing `shouldBe` ["http://localhost:3000"]

    it "reads a single origin" $
      parseOrigins (Just "https://app.onrender.com")
        `shouldBe` ["https://app.onrender.com"]

    it "splits a comma-separated list" $
      parseOrigins (Just "https://a.example,https://b.example")
        `shouldBe` ["https://a.example", "https://b.example"]

    it "trims surrounding whitespace" $
      parseOrigins (Just " https://a.example , https://b.example ")
        `shouldBe` ["https://a.example", "https://b.example"]

    it "discards empty entries" $
      parseOrigins (Just "https://a.example,,")
        `shouldBe` ["https://a.example"]
