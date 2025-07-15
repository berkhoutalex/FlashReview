{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           API
import           Network.HTTP.Types          (methodDelete, methodGet,
                                              methodOptions, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Header   (hAuthorization, hContentType)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           Server                      (initializeApp, server)

-- API with JWT authentication
api :: Proxy FlashcardAPI
api = Proxy

main :: IO ()
main = do

  env <- initializeApp

  let corsPolicy = simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType, hAuthorization],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
        corsOrigins = Nothing  -- Allow any origin
      }

  let app = cors (const $ Just corsPolicy) $ serve api (server env)

  putStrLn "Starting server on http://localhost:8081"
  run 8081 app
