{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import           API
import           Config                      (parseOrigins, resolvePort)
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
import           System.Environment          (lookupEnv)

apiProxy :: Proxy (FlashcardAPI '[JWT])
apiProxy = Proxy

main :: IO ()
main = do
  env <- initializeApp

  port    <- resolvePort  <$> lookupEnv "PORT"
  origins <- parseOrigins <$> lookupEnv "ALLOWED_ORIGINS"

  -- hAuthorization must stay in corsRequestHeaders: it is what permits the
  -- frontend to send its bearer token cross-origin.
  let corsPolicy = simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType, hAuthorization],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
        corsOrigins = Just (origins, True)
      }

  let cookieSett = appCookieSettings env
  let jwtSett = appJWTSettings env

  let cfg = cookieSett :. jwtSett :. EmptyContext

  putStrLn $ "Listening on port " <> show port

  run port $ cors (const $ Just corsPolicy) $
    serveWithContext
      apiProxy cfg $ server env
