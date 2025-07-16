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

  -- Defining a CORS policy with debugging information
  putStrLn "Setting up CORS policy..."
  let corsPolicy = simpleCorsResourcePolicy {
        corsRequestHeaders = [hContentType, hAuthorization],
        corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
        corsOrigins = Just ([ BS.pack "http://localhost:3000" ], True)
      }
  putStrLn $ "CORS allowed headers: Content-Type, Authorization"
  putStrLn $ "CORS allowed origin: http://localhost:3000"


  let port = 8081
  let cookieSett = appCookieSettings env
  let jwtSett = appJWTSettings env

  -- Log cookie settings for debugging
  putStrLn "Cookie settings:"
  putStrLn $ "  cookieIsSecure: " ++ show (cookieIsSecure cookieSett)
  putStrLn $ "  cookieSameSite: " ++ show (cookieSameSite cookieSett)
  putStrLn $ "  cookieXsrfSetting: " ++ show (cookieXsrfSetting cookieSett)

  let cfg = cookieSett :. jwtSett :. EmptyContext

  putStrLn "\n=== SERVER STARTUP ===\n"
  putStrLn $ "Starting server on port " ++ show port
  putStrLn "JWT Authentication is enabled"
  putStrLn "CORS is configured"
  putStrLn "Ensure your frontend sends the JWT token in the Authorization header as 'Bearer <token>'"
  putStrLn "\n===========================\n"

  run port $ cors (const $ Just corsPolicy) $
    serveWithContext
      apiProxy cfg $ server env

