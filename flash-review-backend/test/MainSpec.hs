module MainSpec (spec) where

import qualified APISpec
import qualified ConfigSpec
import qualified DatabaseSpec
import qualified ServerSpec
import           Test.Hspec

spec :: Spec
spec = do
  describe "FlashReview Backend Tests" $ do
    describe "Database Tests" DatabaseSpec.spec
    describe "API Tests" APISpec.spec
    describe "Server Tests" ServerSpec.spec
    describe "Config Tests" ConfigSpec.spec
