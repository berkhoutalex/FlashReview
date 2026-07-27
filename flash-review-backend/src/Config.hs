{-# LANGUAGE OverloadedStrings #-}

-- | Environment-driven configuration for the executable. Lives in the library
-- rather than in Main so that it can be tested.
module Config
  ( resolvePort
  , parseOrigins
  ) where

import           Data.ByteString  (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char        (isSpace)
import           Text.Read        (readMaybe)

-- | Render assigns the listening port via @PORT@.
resolvePort :: Maybe String -> Int
resolvePort mPort = case mPort >>= readMaybe of
  Just p  -> p
  Nothing -> 8081

-- | @ALLOWED_ORIGINS@ is a comma-separated list of origins permitted by CORS.
-- The frontend's Render URL is only known after it is first deployed.
parseOrigins :: Maybe String -> [ByteString]
parseOrigins Nothing = ["http://localhost:3000"]
parseOrigins (Just raw) =
  case filter (not . BS8.null) (map (BS8.pack . trim) (splitOn ',' raw)) of
    []      -> ["http://localhost:3000"]
    origins -> origins
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
      (before, [])        -> [before]
      (before, _ : after) -> before : splitOn c after
