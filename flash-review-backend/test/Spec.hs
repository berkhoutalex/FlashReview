{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import           API
import           Data.Aeson                 (decode, encode)
import           Data.Time.Clock            (getCurrentTime)
import qualified Data.UUID.V4               as UUID
import           Database
import qualified Database.PostgreSQL.Simple as PG
import           Prelude                    hiding (id)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "FlashReview Backend Tests" $ do
    describe "Database Operations" $ do
      it "should set up schema" $ do
        conn <- connectDb
        setupSchema conn
        [PG.Only n] <- PG.query_ conn "SELECT count(*) FROM information_schema.tables WHERE table_name = 'flashcards'"
        n `shouldBe` (1 :: Int)
        PG.close conn

      it "should create a user" $ do
        conn <- connectDb
        userId <- UUID.nextRandom
        let user = User
              { userId = userId
              , username = "testuser"
              , email = "test@example.com"
              , password = "password"
              }
        createdUser <- signupUserDb conn user
        username createdUser `shouldBe` "testuser"
        PG.close conn

      it "should authenticate a user" $ do
        conn <- connectDb
        userId <- UUID.nextRandom
        let user = User
              { userId = userId
              , username =  "authuser"
              , email =  "auth@example.com"
              , password =  "password"
              }
        _ <- signupUserDb conn user

        mUser <- authenticateUserDb conn "authuser" "password"
        case mUser of
          Nothing -> expectationFailure "User authentication failed"
          Just u  -> username u `shouldBe`  "authuser"
        PG.close conn

      it "should create and retrieve a flashcard" $ do
        conn <- connectDb
        userId <- UUID.nextRandom
        cardId <- UUID.nextRandom
        now <- getCurrentTime

        let user = User
              { userId = userId
              , username =   "carduser"
              , email =  "card@example.com"
              , password =  "password"
              }
        _ <- signupUserDb conn user

        let card = Flashcard
              { id = cardId
              , front =  "Test Front"
              , back =  "Test Back"
              , nextReview = now
              , interval = 1
              , easeFactor = 2.5
              , repetitions = 0
              , ownerId = userId
              }

        _ <- createCardDb conn card

        cards <- getAllCardsDb conn userId
        length cards `shouldBe` 1
        PG.close conn

    describe "API Data Types" $ do
      it "should encode and decode Flashcard" $ do
        uuid <- UUID.nextRandom
        ownerUuid <- UUID.nextRandom
        now <- getCurrentTime

        let card = Flashcard
              { id = uuid
              , front =  "Test Front"
              , back =   "Test Back"
              , nextReview = now
              , interval = 1
              , easeFactor = 2.5
              , repetitions = 0
              , ownerId = ownerUuid
              }

        let encoded = encode card
        let decoded = decode encoded :: Maybe Flashcard

        decoded `shouldBe` Just card
