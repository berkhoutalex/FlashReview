{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           API
import qualified Data.ByteString.Char8       as BS
import           Network.HTTP.Types          (methodDelete, methodGet,
                                              methodOptions, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Header   (hAuthorization, hContentType)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.Auth.Server
import           Server                      (AppEnv (..), initializeApp,
                                              server)
apiProxy :: Proxy (FlashcardAPI '[JWT, Cookie])
apiProxy = Proxy

main :: IO ()
main = do

  env <- initializeApp


  let corsPolicy = simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType, hAuthorization],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
        corsOrigins = Just ([ BS.pack "http://localhost:3000" ], True)
      }


  let port = 8081
  let cookieSett = appCookieSettings env
  let jwtSett = appJWTSettings env

  let cfg = cookieSett :. jwtSett :. EmptyContext

  run port $ cors (const $ Just corsPolicy) $
    serveWithContext
      apiProxy cfg $ server env

