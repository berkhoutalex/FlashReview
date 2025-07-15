{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           API
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BSC
import           GHC.Generics
import           Network.HTTP.Types          (methodDelete, methodGet,
                                              methodOptions, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Header   (hAuthorization, hContentType)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Auth.Server
import           Server                      (initializeApp, server)

api :: Proxy (FlashcardAPI '[JWT])
api = Proxy

main :: IO ()
main = do

  env <- initializeApp

  let corsPolicy = simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType, hAuthorization],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
        corsOrigins = Nothing  -- Allow any origin
      }

  let port = 8080
  jwtSecretKey <- generateKey
  let jwtSett = defaultJWTSettings jwtSecretKey
  let cookieSett = defaultCookieSettings
  let cfg = cookieSett :. jwtSett :. EmptyContext
  run port $ serveWithContext
      (Proxy :: Proxy (FlashcardAPI '[JWT])) cfg $ server env

