-- | Small helpers shared across the test suite.
module Test.Support
  ( mkDateTime
  , parseJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either, fromRight')
import Data.Enum (class BoundedEnum, toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

-- | Build a `DateTime` from plain `Int`s. Crashes on out-of-range components,
-- | which is what we want in tests: a bad literal is a broken test, not a
-- | condition to handle.
mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> DateTime
mkDateTime year month day hour minute second millisecond =
  DateTime
    (canonicalDate (unsafeToEnum year) (unsafeToEnum month) (unsafeToEnum day))
    (Time (unsafeToEnum hour) (unsafeToEnum minute) (unsafeToEnum second) (unsafeToEnum millisecond))

unsafeToEnum :: forall a. BoundedEnum a => Int -> a
unsafeToEnum n = unsafePartial fromJust (toEnum n)

-- | Parse a JSON literal written inline in a test. Crashes if the literal is
-- | malformed, since that is a mistake in the test rather than a case under test.
parseJson :: String -> Json
parseJson str =
  fromRight' (\_ -> unsafeCrashWith ("Test wrote malformed JSON: " <> str))
    (jsonParser str :: Either String Json)
