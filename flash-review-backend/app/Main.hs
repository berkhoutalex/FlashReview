{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           API
import           Network.HTTP.Types          (methodDelete, methodGet,
                                              methodOptions, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Header   (hContentType)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           Server                      (initializeApp, server)

api :: Proxy FlashcardAPI
api = Proxy

main :: IO ()
main = do
  putStrLn "Starting flashcards backend on port 8081"

  -- Initialize app environment with database connection
  env <- initializeApp

  -- Create the application with CORS middleware
  let app = cors ( const $ Just (simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
      }) ) $ serve api (server env)

  putStrLn "Starting server on http://localhost:8081"
  run 8081 app
