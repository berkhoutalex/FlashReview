{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module APISpec (spec) where

import           API
import           Data.Aeson      (decode, encode)
import           Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4    as UUID
import           Prelude         hiding (id)
import           Test.Hspec

spec :: Spec
spec = do
  describe "API Data Types" $ do
    describe "Flashcard JSON Serialization" $ do
      it "should encode and decode Flashcard" $ do
        uuid <- UUID.nextRandom
        ownerUuid <- UUID.nextRandom
        now <- getCurrentTime

        let card = Flashcard
              { id = uuid
              , front =  "Test Front"
              , back =  "Test Back"
              , nextReview = now
              , interval = 1
              , easeFactor = 2.5
              , repetitions = 0
              , ownerId = ownerUuid
              }

        let encoded = encode card
        let decoded = decode encoded :: Maybe Flashcard

        decoded `shouldBe` Just card

    describe "FlashcardRequest JSON Serialization" $ do
      it "should encode and decode FlashcardRequest" $ do
        uuid <- UUID.nextRandom
        now <- getCurrentTime

        let request = FlashcardRequest
              { reqId = uuid
              , reqFront =  "Test Front"
              , reqBack =  "Test Back"
              , reqNextReview = now
              , reqInterval = 1
              , reqEaseFactor = 2.5
              , reqRepetitions = 0
              }

        let encoded = encode request
        let decoded = decode encoded :: Maybe FlashcardRequest

        decoded `shouldBe` Just request

    describe "ReviewResult JSON Serialization" $ do
      it "should encode and decode ReviewResult" $ do
        let review = ReviewResult { rating = 4 }

        let encoded = encode review
        let decoded = decode encoded :: Maybe ReviewResult

        decoded `shouldBe` Just review

    describe "User JSON Serialization" $ do
      it "should encode and decode User" $ do
        uuid <- UUID.nextRandom

        let user = User
              { userId = uuid
              , username =  "testuser"
              , email =  "test@example.com"
              , password =  "password"
              }

        let encoded = encode user
        let decoded = decode encoded :: Maybe User

        decoded `shouldBe` Just user

    describe "UserJWT JSON Serialization" $ do
      it "should encode and decode UserJWT" $ do
        uuid <- UUID.nextRandom

        let userJwt = UserJWT
              { userJwtId = uuid
              , userJwtName =  "testuser"
              , userJwtEmail =  "test@example.com"
              }

        let encoded = encode userJwt
        let decoded = decode encoded :: Maybe UserJWT

        decoded `shouldBe` Just userJwt

    describe "SignupRequest JSON Serialization" $ do
      it "should encode and decode SignupRequest" $ do
        let signup = SignupRequest
              { signupUsername =  "testuser"
              , signupEmail =  "test@example.com"
              , signupPassword =  "password"
              }

        let encoded = encode signup
        let decoded = decode encoded :: Maybe SignupRequest

        decoded `shouldBe` Just signup

    describe "LoginRequest JSON Serialization" $ do
      it "should encode and decode LoginRequest" $ do
        let login = LoginRequest
              { loginUsername =  "testuser"
              , loginPassword =  "password"
              }

        let encoded = encode login
        let decoded = decode encoded :: Maybe LoginRequest

        decoded `shouldBe` Just login

    describe "flashcardToRequest" $ do
      it "should convert Flashcard to FlashcardRequest" $ do
        uuid <- UUID.nextRandom
        ownerUuid <- UUID.nextRandom
        now <- getCurrentTime

        let card = Flashcard
              { id = uuid
              , front =  "Test Front"
              , back =  "Test Back"
              , nextReview = now
              , interval = 1
              , easeFactor = 2.5
              , repetitions = 0
              , ownerId = ownerUuid
              }

        let request = flashcardToRequest card

        reqId request `shouldBe` id card
        reqFront request `shouldBe` front card
        reqBack request `shouldBe` back card
        reqNextReview request `shouldBe` nextReview card
        reqInterval request `shouldBe` interval card
        reqEaseFactor request `shouldBe` easeFactor card
        reqRepetitions request `shouldBe` repetitions card
