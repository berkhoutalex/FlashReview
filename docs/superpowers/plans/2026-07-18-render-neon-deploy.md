# Render + Neon Deployment Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Host the Haskell backend and PureScript frontend on Render's free tier with Postgres on Neon, replacing cookie auth with bearer tokens and the single DB connection with a pool.

**Architecture:** Two Render services from one monorepo — a Docker Web Service for the backend (root dir `flash-review-backend`) and a Static Site for the frontend (root dir `flash-review-frontend`) — talking to a Neon Postgres instance over a pooled connection string. Auth moves from cross-site cookies to `Authorization: Bearer` headers, since the two services land on different domains.

**Tech Stack:** Haskell (GHC 9.8.4, Stack LTS 23.26), servant / servant-auth-server 0.4.9.0, postgresql-simple, resource-pool 0.4.0.0, PureScript 0.15.15 + Halogen, Spago, Docker.

**Spec:** `docs/superpowers/specs/2026-07-18-render-neon-deploy-design.md`

## Global Constraints

- GHC version is **9.8.4**, from Stack snapshot **lts-23.26**. The Docker build image must match.
- `resource-pool` is **0.4.0.0**. Use `newPool :: PoolConfig a -> IO (Pool a)` and `defaultPoolConfig :: IO a -> (a -> IO ()) -> Double -> Int -> PoolConfig a`. The pre-0.4 `createPool` API does **not** exist here.
- `servant-auth-server` is **0.4.9.0**, which exports `fromSecret :: ByteString -> JWK`.
- Pool limits: **max 5 connections, 30s idle timeout**. Both must stay under Neon free-tier limits.
- The localStorage key for the JWT is exactly **`frToken`**, used identically in `Storage.js` and any test.
- Backend must read its port from **`PORT`**, CORS origins from **`ALLOWED_ORIGINS`** (comma-separated), the database from **`DATABASE_URL`**, and the JWT signing secret from **`JWT_SECRET`**.
- Local-dev defaults must keep working when none of those variables are set: port `8081`, origin `http://localhost:3000`, the existing `PG*` variables, and an ephemeral generated JWT key.
- Every backend task runs the full suite with `stack test`; it must be green before the commit step.

## Environment

Verified working on 2026-07-18. The baseline suite passes (5 examples, 0 failures) before any task begins.

**Stack is at `C:\ghcup\bin`, which is NOT on PATH.** Every shell that runs a stack command must prepend it first, or `stack` will appear not to exist:

```powershell
$env:PATH = "C:\ghcup\bin;$env:PATH"
```

Stack is 3.11.1; GHC 9.8.4 is installed and matches lts-23.26. The dependency tree is already compiled — `stack build` and `stack test` are fast from here.

**Postgres is the native `postgresql-x64-17` Windows service** (auto-start, listening on 5432 over both IPv4 and IPv6), with credentials `postgres`/`postgres`. Databases `flashcards` and `flashcards_test` both exist.

Do **not** run `docker-compose up` for Postgres on this machine. The native service already holds port 5432, so the container starts, reports healthy, and binds nothing — any database created inside it is invisible to the tests, which produces a confusing "database does not exist" failure against a server that is plainly running.

`psql` is available at:

```
C:\Users\cindy\AppData\Local\Programs\stack\x86_64-windows\msys2-20240727\clang64\bin\psql.exe
```

Test environment variables:

```
PGHOST=localhost PGUSER=postgres PGPASSWORD=postgres PGDATABASE=flashcards_test
```

**One build-environment note:** `postgresql-libpq` needs libpq to compile. It was installed into Stack's MSYS2 via `pacman -S mingw-w64-clang-x86_64-postgresql`. If a clean rebuild ever fails at `postgresql-libpq-configure`, that package is missing.

Expect to fix small compile errors in the Haskell code below. The types and signatures were verified against Hackage/Stackage documentation for the exact snapshot, and `resource-pool-0.4.0.0`, `servant-auth-server-0.4.9.0`, and GHC 9.8.4 were each confirmed against the resolved build plan — but the code itself was never compiled.

---

## Task 0: Revive the dormant test suite

`test/Spec.hs` is the test entrypoint and never imports `MainSpec`, the module aggregating `APISpec`, `DatabaseSpec`, and `ServerSpec`. It re-implements ~5 tests inline instead. The eight HTTP tests in `ServerSpec.hs` are compiled but never run — including every test covering the handlers later tasks modify.

Two defects are predicted by inspection and should surface when the suite runs. They are predictions, not observations; if reality differs, trust reality and adjust.

**Files:**
- Modify: `flash-review-backend/test/Spec.hs` (replace entirely)
- Modify: `flash-review-backend/test/ServerSpec.hs:68,95`
- Modify: `flash-review-backend/test/DatabaseSpec.hs:100-111`

**Interfaces:**
- Consumes: nothing.
- Produces: a genuinely executing test suite. Every later task depends on `stack test` being meaningful. Also produces the top-level binding `testKey :: JWK` inside `test/ServerSpec.hs`. Tasks 3 and 4 both edit that same module in place, so `testKey` is already in scope for them — it does **not** need to be added to `ServerSpec`'s export list.

- [ ] **Step 1: Replace `test/Spec.hs` with a delegation to `MainSpec`**

This discards the inline tests. That loses no coverage: `DatabaseSpec` strictly supersedes the inline database tests and `APISpec` supersedes the inline serialization test.

```haskell
module Main where

import qualified MainSpec
import           Test.Hspec

main :: IO ()
main = hspec MainSpec.spec
```

- [ ] **Step 2: Run the suite and observe the failures**

```bash
cd flash-review-backend
stack test
```

Expected: the suite now runs `DatabaseSpec`, `APISpec`, and `ServerSpec`. Predicted failures:

- `Server Tests` — every test sending an `Authorization` header (create/retrieve, update, delete, review queue, submit review, stats) fails with `401` where `200` or `204` is expected. Cause: `makeTestApp` calls `generateKey` at line 68 and `getTestToken` calls `generateKey` again at line 95, producing two different keys, so tokens are signed with one and validated against the other.
- `Database Tests` — "should update a flashcard" fails. Cause: the test never calls `updateCardDb`; it creates a card, builds an `updatedCard` value, then asserts the database returns the updated version.

Record the actual failure list before continuing.

- [ ] **Step 3: Fix the ServerSpec key mismatch**

Replace both `generateKey` calls with a single shared, deterministic key. Add this top-level binding to `test/ServerSpec.hs` (after the `testConfig` definition):

```haskell
testKey :: JWK
testKey = fromSecret (BS8.pack "flashreview-test-secret-not-for-production-use")
```

In `makeTestApp`, replace:

```haskell
  myKey <- generateKey
  let jwtSettings = defaultJWTSettings myKey
```

with:

```haskell
  let jwtSettings = defaultJWTSettings testKey
```

In `getTestToken`, replace:

```haskell
      myKey <- generateKey
      let jwtSettings = defaultJWTSettings myKey
```

with:

```haskell
      let jwtSettings = defaultJWTSettings testKey
```

`fromSecret` and `JWK` both come from `Servant.Auth.Server`, already imported at line 29. `BS8` is already imported at line 10.

- [ ] **Step 4: Fix the DatabaseSpec update test**

In `test/DatabaseSpec.hs`, the "should update a flashcard" test is missing its call to the function under test. Replace lines 100-111 with:

```haskell
      it "should update a flashcard" $ \conn -> do
        setupTestDb conn
        user <- createTestUser conn
        card <- createTestFlashcard (userId user)

        _ <- createCardDb conn card

        let updatedCard = card { front =  "Updated Front", back =  "Updated Back" }

        _ <- updateCardDb conn (id card) updatedCard

        mCard <- getCardByIdDb conn (id card) (userId user)
        mCard `shouldBe` Just updatedCard
```

- [ ] **Step 5: Run the suite and verify it is green**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass. If other dormant failures surfaced in Step 2 that these two fixes do not address, fix them now — the suite must be green before any migration work starts. If a failure reveals a genuine product bug rather than a test bug, stop and report it rather than editing the assertion to match the behavior.

- [ ] **Step 6: Commit**

```bash
git add flash-review-backend/test/Spec.hs flash-review-backend/test/ServerSpec.hs flash-review-backend/test/DatabaseSpec.hs
git commit -m "test: wire MainSpec into the test entrypoint and fix two dormant failures

Spec.hs never imported MainSpec, so APISpec, DatabaseSpec, and ServerSpec
were compiled but never executed. Running them surfaced a JWT key mismatch
in ServerSpec and a missing updateCardDb call in DatabaseSpec."
```

---

## Task 1: Read the database connection from `DATABASE_URL`

Neon supplies one connection string carrying `sslmode=require`; the current code assembles one from five separate `PG*` variables and never requests TLS.

**Files:**
- Modify: `flash-review-backend/src/Database.hs` (export list, `connectDb`)
- Modify: `flash-review-backend/test/DatabaseSpec.hs` (add tests)

**Interfaces:**
- Consumes: `makeConnectionString :: DatabaseConfig -> ByteString` (existing).
- Produces: `resolveConnectionString :: Maybe String -> DatabaseConfig -> ByteString`. Task 2 relies on `connectDb :: IO PG.Connection` keeping its current signature.

- [ ] **Step 1: Write the failing tests**

Add to `test/DatabaseSpec.hs`, inside `spec`, as a new `describe` block at the same level as the existing `around withTestConnection` block (not inside it — these need no connection):

```haskell
    describe "Connection string resolution" $ do
      it "uses DATABASE_URL verbatim when present" $ do
        let url = "postgresql://u:p@ep-x-pooler.neon.tech/db?sslmode=require"
        resolveConnectionString (Just url) testConfig
          `shouldBe` BS8.pack url

      it "falls back to the PG* config when DATABASE_URL is absent" $ do
        resolveConnectionString Nothing testConfig
          `shouldBe` makeConnectionString testConfig
```

Add this import to `test/DatabaseSpec.hs`:

```haskell
import qualified Data.ByteString.Char8      as BS8
```

`spec`'s structure becomes:

```haskell
spec :: Spec
spec = do
  around withTestConnection $ do
    describe "Database Operations" $ do
      ...existing tests unchanged...

  describe "Connection string resolution" $ do
    ...the two new tests...
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd flash-review-backend
stack test
```

Expected: compile error, `Variable not in scope: resolveConnectionString`.

- [ ] **Step 3: Implement `resolveConnectionString` and rewire `connectDb`**

In `src/Database.hs`, add `resolveConnectionString` to the export list:

```haskell
  , makeConnectionString
  , resolveConnectionString
  ) where
```

Add the function next to `makeConnectionString`:

```haskell
-- | Neon supplies a single connection URL that already carries @sslmode=require@.
-- Local development and CI supply the individual @PG*@ variables instead.
resolveConnectionString :: Maybe String -> DatabaseConfig -> ByteString
resolveConnectionString (Just url) _   = BS.pack url
resolveConnectionString Nothing    cfg = makeConnectionString cfg
```

Replace `connectDb`:

```haskell
connectDb :: IO PG.Connection
connectDb = do
  putStrLn "Connecting to PostgreSQL database..."

  mUrl <- lookupEnv "DATABASE_URL"
  config <- loadConfig

  let connStr = resolveConnectionString mUrl config

  connectionResult <- try (PG.connectPostgreSQL connStr)
  case connectionResult of
    Left (e :: SomeException) -> do
      throwIO e
    Right conn -> do
      putStrLn "Connected to PostgreSQL successfully!"
      return conn
```

`lookupEnv` is already imported at line 46.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass, including the two new ones.

- [ ] **Step 5: Commit**

```bash
git add flash-review-backend/src/Database.hs flash-review-backend/test/DatabaseSpec.hs
git commit -m "feat: support DATABASE_URL with PG* fallback

Neon supplies one connection string with sslmode=require. The PG* path is
kept so local dev, docker-compose, and CI work unchanged."
```

---

## Task 2: Add a connection pool

Neon scales compute to zero and drops idle connections. A single long-lived `PG.Connection` has no reconnect path, so the first request after idle fails and stays failing.

**Files:**
- Modify: `flash-review-backend/src/Database.hs` (export list, imports, `mkPool`)
- Modify: `flash-review-backend/test/DatabaseSpec.hs` (add tests)

**Interfaces:**
- Consumes: `connectDb :: IO PG.Connection` (Task 1).
- Produces: `mkPool :: IO (Pool PG.Connection)`. Task 3 stores this in `AppEnv`.

- [ ] **Step 1: Write the failing tests**

Add to `test/DatabaseSpec.hs` as another top-level `describe` inside `spec`:

```haskell
  describe "Connection pool" $ do
    it "serves a working connection" $ do
      pool <- mkPool
      n <- withResource pool $ \conn ->
        PG.query_ conn "SELECT 1 :: int"
      n `shouldBe` [PG.Only (1 :: Int)]
      destroyAllResources pool

    it "serves connections across sequential acquisitions" $ do
      pool <- mkPool
      a <- withResource pool $ \conn -> PG.query_ conn "SELECT 1 :: int"
      b <- withResource pool $ \conn -> PG.query_ conn "SELECT 2 :: int"
      a `shouldBe` [PG.Only (1 :: Int)]
      b `shouldBe` [PG.Only (2 :: Int)]
      destroyAllResources pool
```

Add the import:

```haskell
import           Data.Pool                  (destroyAllResources, withResource)
```

These tests connect using the ambient environment rather than `testConfig`, because `mkPool` builds its connections from `connectDb`. They issue only `SELECT 1` / `SELECT 2`, so *which* database they land in does not matter — but one must exist and be reachable. CI already sets `PGDATABASE=flashcards_test` in `backend-tests.yml`; locally, the default `flashcards` database from `docker-compose.yml` is fine.

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd flash-review-backend
stack test
```

Expected: compile error, `Variable not in scope: mkPool`.

- [ ] **Step 3: Implement `mkPool`**

In `src/Database.hs`, add to the export list:

```haskell
  , mkPool
  , Pool
```

Add the import:

```haskell
import           Data.Pool                          (Pool, defaultPoolConfig,
                                                     newPool, setNumStripes)
```

Add the function after `connectDb`:

```haskell
-- | Neon's free tier drops idle connections when it scales compute to zero.
-- A 30s idle timeout retires pooled connections before Neon does, and a single
-- stripe keeps the 5-connection cap exact rather than per-stripe.
mkPool :: IO (Pool PG.Connection)
mkPool = newPool
  $ setNumStripes (Just 1)
  $ defaultPoolConfig connectDb PG.close 30 5
```

Argument order for `defaultPoolConfig` is: create action, destroy action, idle timeout in seconds, max resources.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add flash-review-backend/src/Database.hs flash-review-backend/test/DatabaseSpec.hs
git commit -m "feat: add a postgres connection pool

Max 5 connections, 30s idle timeout, single stripe. resource-pool was
already a declared dependency but unused."
```

---

## Task 3: Move `AppEnv` from a bare connection to the pool

**Files:**
- Modify: `flash-review-backend/src/Server.hs` (`AppEnv`, `initializeApp`, all 9 handlers)
- Modify: `flash-review-backend/test/ServerSpec.hs:52-84` (`makeTestApp`)

**Interfaces:**
- Consumes: `mkPool :: IO (Pool PG.Connection)` (Task 2).
- Produces: `AppEnv { appDbPool :: Pool PG.Connection, appCookieSettings :: CookieSettings, appJWTSettings :: JWTSettings }`. Task 4 modifies `initializeApp`; Task 5's `Main.hs` constructs the app from it.

The existing `ServerSpec` tests are the coverage for this task — they exercise all nine handlers over HTTP. They must stay green. No new tests are needed; this is a refactor with behavior held constant.

- [ ] **Step 1: Change `AppEnv` and `initializeApp`**

In `src/Server.hs`, add the import:

```haskell
import           Data.Pool                  (Pool, withResource)
```

Replace the `AppEnv` definition:

```haskell
data AppEnv = AppEnv
  { appDbPool         :: Pool PG.Connection
  , appCookieSettings :: CookieSettings
  , appJWTSettings    :: JWTSettings
  }
```

Replace `initializeApp`:

```haskell
initializeApp :: IO AppEnv
initializeApp = do
  pool <- DB.mkPool
  withResource pool DB.setupSchema
  myKey <- generateKey

  let jwtSettings = defaultJWTSettings myKey

  let cookieSettings = defaultCookieSettings {
        cookieIsSecure = NotSecure,
        cookieXsrfSetting = Nothing
      }

  pure $ AppEnv pool cookieSettings jwtSettings
```

- [ ] **Step 2: Rewrite the nine handlers to acquire from the pool**

Each handler replaces its direct `appDbConn` use with `withResource appDbPool`. The full set:

```haskell
getCards :: AuthResult API.UserJWT -> AppEnv -> Handler [API.Flashcard]
getCards authResult AppEnv{..} =
  case authResult of
    Authenticated user ->
      liftIO $ withResource appDbPool $ \conn ->
        DB.getAllCardsDb conn (API.userJwtId user)
    _ -> throwError err401

createCard :: AuthResult API.UserJWT -> AppEnv -> API.FlashcardRequest -> Handler API.Flashcard
createCard authResult AppEnv{..} flashcardReq =
  case authResult of
    Authenticated user ->
      let flashcard = API.Flashcard
            { API.id          = API.reqId flashcardReq
            , API.front       = API.reqFront flashcardReq
            , API.back        = API.reqBack flashcardReq
            , API.nextReview  = API.reqNextReview flashcardReq
            , API.interval    = API.reqInterval flashcardReq
            , API.easeFactor  = API.reqEaseFactor flashcardReq
            , API.repetitions = API.reqRepetitions flashcardReq
            , API.ownerId     = API.userJwtId user
            }
      in liftIO $ withResource appDbPool $ \conn ->
           DB.createCardDb conn flashcard
    _ -> throwError err401

updateCard :: AuthResult API.UserJWT -> AppEnv -> UUID -> API.FlashcardRequest -> Handler API.Flashcard
updateCard authResult AppEnv{..} uuid flashcardReq =
  case authResult of
    Authenticated user ->
      let flashcard = API.Flashcard
            { API.id          = uuid
            , API.front       = API.reqFront flashcardReq
            , API.back        = API.reqBack flashcardReq
            , API.nextReview  = API.reqNextReview flashcardReq
            , API.interval    = API.reqInterval flashcardReq
            , API.easeFactor  = API.reqEaseFactor flashcardReq
            , API.repetitions = API.reqRepetitions flashcardReq
            , API.ownerId     = API.userJwtId user
            }
      in liftIO $ withResource appDbPool $ \conn ->
           DB.updateCardDb conn uuid flashcard
    _ -> throwError err401

deleteCard :: AuthResult API.UserJWT -> AppEnv -> UUID -> Handler NoContent
deleteCard authResult AppEnv{..} uuid =
  case authResult of
    Authenticated user -> do
      liftIO $ withResource appDbPool $ \conn ->
        DB.deleteCardDb conn uuid (API.userJwtId user)
      pure NoContent
    _ -> throwError err401

getReviewQueue :: AuthResult API.UserJWT -> AppEnv -> Handler [API.Flashcard]
getReviewQueue authResult AppEnv{..} =
  case authResult of
    Authenticated user ->
      liftIO $ withResource appDbPool $ \conn ->
        DB.getReviewCardsDb conn (API.userJwtId user)
    _ -> throwError err401

submitReview :: AuthResult API.UserJWT -> AppEnv -> UUID -> API.ReviewResult -> Handler NoContent
submitReview authResult AppEnv{..} uuid result =
  case authResult of
    Authenticated user -> do
      liftIO $ withResource appDbPool $ \conn ->
        DB.processReviewDb conn uuid (API.userJwtId user) result
      pure NoContent
    _ -> throwError err401

getStats :: AuthResult API.UserJWT -> AppEnv -> Handler API.Stats
getStats authResult AppEnv{..} =
  case authResult of
    Authenticated user -> do
      dueCount <- liftIO $ withResource appDbPool $ \conn ->
        DB.getDueCountDb conn (API.userJwtId user)
      pure $ API.Stats dueCount
    _ -> throwError err401
```

For `userLogin`, replace only the database call:

```haskell
  mUser <- liftIO $ withResource appDbPool $ \conn ->
    DB.authenticateUserDb conn (API.loginUsername loginReq) (API.loginPassword loginReq)
```

For `userSignup`, replace only the database call:

```haskell
  liftIO $ withResource appDbPool $ \conn -> DB.signupUserDb conn user
```

The rest of both functions is unchanged.

- [ ] **Step 3: Update `makeTestApp` in ServerSpec**

The test builds `AppEnv` directly. It needs a pool over the test database rather than the ambient environment, so it constructs one explicitly instead of calling `DB.mkPool`.

In `test/ServerSpec.hs`, add imports:

```haskell
import           Data.Pool                   (defaultPoolConfig, newPool,
                                              setNumStripes, withResource)
```

Replace `makeTestApp` lines 52-84 with:

```haskell
makeTestApp :: IO Application
makeTestApp = do
  let connStr = DB.makeConnectionString testConfig
  pool <- newPool
    $ setNumStripes (Just 1)
    $ defaultPoolConfig (PG.connectPostgreSQL connStr) PG.close 30 5

  withResource pool DB.setupSchema

  userId <- UUID.nextRandom
  let user = User
        { userId = userId
        , username =  "testuser"
        , email =  "test@example.com"
        , password =  "password"
        }
  _ <- withResource pool $ \conn -> DB.signupUserDb conn user

  let jwtSettings = defaultJWTSettings testKey
  let cookieSettings = defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieXsrfSetting = Nothing
        }

  let env = AppEnv pool cookieSettings jwtSettings
  let corsPolicy = simpleCorsResourcePolicy
        { corsRequestHeaders = [hContentType, hAuthorization]
        , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
        }

  return $ cors (const $ Just corsPolicy) $ serveWithContext
    (Proxy :: Proxy (FlashcardAPI '[JWT]))
    (cookieSettings :. jwtSettings :. EmptyContext)
    (server env)
```

Note this already uses `testKey` from Task 0.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass, unchanged from Task 2. The nine ServerSpec HTTP tests are what prove the pool refactor preserved behavior.

- [ ] **Step 5: Commit**

```bash
git add flash-review-backend/src/Server.hs flash-review-backend/test/ServerSpec.hs
git commit -m "refactor: acquire connections from the pool per request

AppEnv holds a Pool rather than a single Connection. Database.hs is
unchanged; every function there already took a Connection."
```

---

## Task 4: Derive the JWT signing key from `JWT_SECRET`

`initializeApp` calls `generateKey` on every boot. Render's free tier spins the service down after 15 minutes idle, so every cold start invalidates every outstanding token and silently logs everyone out.

**Files:**
- Modify: `flash-review-backend/src/Server.hs` (export list, imports, `initializeApp`, new `resolveJwtKey`)
- Modify: `flash-review-backend/test/ServerSpec.hs` (add tests)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `resolveJwtKey :: Maybe String -> IO JWK`.

- [ ] **Step 1: Write the failing tests**

The property that matters is that a token signed before a restart still validates after it. Testing JWK equality would be weaker and depends on `JWK`'s `Eq` instance; this tests the actual behavior.

Add to `test/ServerSpec.hs`, inside `spec` at the same level as `describe "Server API"`:

```haskell
  describe "JWT key resolution" $ do
    it "accepts a token across simulated restarts when JWT_SECRET is set" $ do
      let secret = "a-fixed-secret-value"
          claims = UserJWT
            { userJwtId = read "123e4567-e89b-12d3-a456-426614174000"
            , userJwtName = "testuser"
            , userJwtEmail = "test@example.com"
            }

      keyBefore <- Server.resolveJwtKey (Just secret)
      token <- makeJWT claims (defaultJWTSettings keyBefore) Nothing

      keyAfter <- Server.resolveJwtKey (Just secret)
      case token of
        Left err -> expectationFailure ("could not sign: " ++ show err)
        Right t  -> do
          verified <- verifyJWT (defaultJWTSettings keyAfter) (BSL.toStrict t)
          verified `shouldBe` Just claims

    it "rejects a token signed under a different secret" $ do
      let claims = UserJWT
            { userJwtId = read "123e4567-e89b-12d3-a456-426614174000"
            , userJwtName = "testuser"
            , userJwtEmail = "test@example.com"
            }

      keyA <- Server.resolveJwtKey (Just "secret-a")
      keyB <- Server.resolveJwtKey (Just "secret-b")
      token <- makeJWT claims (defaultJWTSettings keyA) Nothing
      case token of
        Left err -> expectationFailure ("could not sign: " ++ show err)
        Right t  -> do
          verified <- verifyJWT (defaultJWTSettings keyB) (BSL.toStrict t)
          verified `shouldBe` Nothing
```

Change the `Server` import at line 25 to bring in the new function:

```haskell
import           Server                      (AppEnv (..), server)
import qualified Server
```

`UserJWT` derives `Eq` (`API.hs:105`), so `shouldBe` works on it. `BSL` is already imported at line 11.

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd flash-review-backend
stack test
```

Expected: compile error, `Variable not in scope: Server.resolveJwtKey`.

- [ ] **Step 3: Implement `resolveJwtKey`**

In `src/Server.hs`, extend the export list:

```haskell
module Server( server, initializeApp, resolveJwtKey, AppEnv(..) ) where
```

Add imports:

```haskell
import           Crypto.JOSE.JWK            (JWK)
import qualified Data.ByteString.Char8      as BS8
import           System.Environment         (lookupEnv)
import           System.IO                  (hPutStrLn, stderr)
```

Add `jose` to the **top-level** `dependencies:` list in `package.yaml` (not just the test suite — `Server.hs` is in the library and `resolveJwtKey`'s signature mentions `JWK`):

```yaml
- bcrypt
- jose
```

This correction comes from Task 0, which hit the same problem: `Servant.Auth.Server` re-exports the *value* `fromSecret` but **not** the *type* `JWK`. `JWK`'s home module is `Crypto.JOSE.JWK` in the `jose` package, which is present transitively via `servant-auth-server` but must be declared to be importable. Task 0 already added `jose` to the test-suite dependencies; this adds it for the library.

Add the function above `initializeApp`:

```haskell
-- | A key generated per-process would invalidate every outstanding token on
-- each cold start, and Render's free tier spins the service down when idle.
resolveJwtKey :: Maybe String -> IO JWK
resolveJwtKey (Just secret) = pure (fromSecret (BS8.pack secret))
resolveJwtKey Nothing = do
  hPutStrLn stderr
    "WARNING: JWT_SECRET is not set; generating an ephemeral signing key. \
    \All sessions will be invalidated when this process restarts."
  generateKey
```

Update `initializeApp` to use it:

```haskell
initializeApp :: IO AppEnv
initializeApp = do
  pool <- DB.mkPool
  withResource pool DB.setupSchema

  mSecret <- lookupEnv "JWT_SECRET"
  myKey <- resolveJwtKey mSecret

  let jwtSettings = defaultJWTSettings myKey

  let cookieSettings = defaultCookieSettings {
        cookieIsSecure = NotSecure,
        cookieXsrfSetting = Nothing
      }

  pure $ AppEnv pool cookieSettings jwtSettings
```

`fromSecret`, `generateKey`, `makeJWT`, and `verifyJWT` come from `Servant.Auth.Server`, already imported. The `JWK` *type* does not — see the import note above.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add flash-review-backend/src/Server.hs flash-review-backend/test/ServerSpec.hs
git commit -m "feat: derive the JWT signing key from JWT_SECRET

Without this every cold start on Render's free tier invalidates all
outstanding tokens. Falls back to an ephemeral key with a warning."
```

---

## Task 5: Read port and CORS origins from the environment

`Main.hs` hardcodes port `8081` and the CORS origin `http://localhost:3000`. Render assigns a port via `PORT`, and the frontend's origin is only known after it is deployed.

`Main.hs` belongs to the executable and cannot be imported by the test suite, so the parsing logic goes in a new library module.

**Files:**
- Create: `flash-review-backend/src/Config.hs`
- Create: `flash-review-backend/test/ConfigSpec.hs`
- Modify: `flash-review-backend/test/MainSpec.hs`
- Modify: `flash-review-backend/app/Main.hs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `resolvePort :: Maybe String -> Int` and `parseOrigins :: Maybe String -> [ByteString]`, both in module `Config`.

- [ ] **Step 1: Write the failing tests**

Create `test/ConfigSpec.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (spec) where

import           Config     (parseOrigins, resolvePort)
import           Test.Hspec

spec :: Spec
spec = do
  describe "resolvePort" $ do
    it "defaults to 8081 when PORT is unset" $
      resolvePort Nothing `shouldBe` 8081

    it "reads a numeric PORT" $
      resolvePort (Just "10000") `shouldBe` 10000

    it "falls back to the default when PORT is not a number" $
      resolvePort (Just "not-a-port") `shouldBe` 8081

  describe "parseOrigins" $ do
    it "defaults to the local dev origin when unset" $
      parseOrigins Nothing `shouldBe` ["http://localhost:3000"]

    it "reads a single origin" $
      parseOrigins (Just "https://app.onrender.com")
        `shouldBe` ["https://app.onrender.com"]

    it "splits a comma-separated list" $
      parseOrigins (Just "https://a.example,https://b.example")
        `shouldBe` ["https://a.example", "https://b.example"]

    it "trims surrounding whitespace" $
      parseOrigins (Just " https://a.example , https://b.example ")
        `shouldBe` ["https://a.example", "https://b.example"]

    it "discards empty entries" $
      parseOrigins (Just "https://a.example,,")
        `shouldBe` ["https://a.example"]
```

Wire it into `test/MainSpec.hs`:

```haskell
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
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd flash-review-backend
stack test
```

Expected: compile error, `Could not find module 'Config'`.

- [ ] **Step 3: Implement the `Config` module**

Create `src/Config.hs`:

```haskell
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
```

`hpack` generates the module list from `source-dirs`, so `Config` is picked up automatically with no `package.yaml` change.

- [ ] **Step 4: Run the tests to verify they pass**

```bash
cd flash-review-backend
stack test
```

Expected: all tests pass, including the six new `Config Tests`.

- [ ] **Step 5: Rewrite `app/Main.hs` to use them**

Replace the whole file:

```haskell
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
```

The proxy narrows from `'[JWT, Cookie]` to `'[JWT]` now that cookie auth is unused. The `Data.ByteString.Char8` import is gone because `parseOrigins` returns `ByteString` already.

- [ ] **Step 6: Verify the executable builds and starts**

```bash
cd flash-review-backend
stack build
PORT=9999 ALLOWED_ORIGINS=https://example.com stack exec flash-review-backend-exe
```

Expected: prints `Listening on port 9999`. Stop it with Ctrl-C. If it cannot reach Postgres, start `docker-compose up -d` first.

- [ ] **Step 7: Commit**

```bash
git add flash-review-backend/src/Config.hs flash-review-backend/test/ConfigSpec.hs flash-review-backend/test/MainSpec.hs flash-review-backend/app/Main.hs
git commit -m "feat: read PORT and ALLOWED_ORIGINS from the environment

Render assigns the port and the frontend origin is only known after the
static site is deployed. Parsing lives in a new Config library module so
it is testable; Main is an executable and cannot be imported by tests."
```

---

## Task 6: Containerize the backend

**Files:**
- Create: `flash-review-backend/Dockerfile`
- Create: `flash-review-backend/.dockerignore`

**Interfaces:**
- Consumes: the executable `flash-review-backend-exe` and the `PORT` handling from Task 5.
- Produces: a runnable image. Task 7's `render.yaml` points at this Dockerfile.

Two corrections to the reference guide's template: the runtime image must include **`libpq5`** (postgresql-simple links against libpq; without it the binary fails at startup) and the build stage needs `libpq-dev` and `zlib1g-dev`.

This task deviates from the spec in one detail: the spec described locating the binary via `stack path --local-install-root`, but `--copy-bins --local-bin-path` puts it at a known path directly and is simpler. Same result, fewer moving parts.

- [ ] **Step 1: Create `.dockerignore`**

Keeps local build artifacts out of the build context, which otherwise bloats it and can poison the build.

```
.stack-work/
pgdata/
*.hi
*.o
docs/
README.md
```

- [ ] **Step 2: Create the Dockerfile**

```dockerfile
# syntax=docker/dockerfile:1

# --- build stage ---
# GHC 9.8.4 matches the compiler in stack snapshot lts-23.26.
FROM haskell:9.8.4 AS build

WORKDIR /app

# libpq-dev is required to link postgresql-simple; zlib1g-dev for http-client.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libpq-dev \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy only the dependency manifests first so this layer caches across source
# changes. Haskell dependency builds are slow and Render has no build cache.
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --system-ghc --only-dependencies

COPY . .
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin

# --- runtime stage ---
FROM debian:bookworm-slim

# libpq5 is the runtime half of libpq-dev and is missing from the reference
# guide's template; without it the server fails to start.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp10 \
      libpq5 \
      zlib1g \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/bin/flash-review-backend-exe /app/server

ENV PORT=10000
EXPOSE 10000

CMD ["/app/server"]
```

- [ ] **Step 3: Build the image**

```bash
cd flash-review-backend
docker build -t flash-review-backend .
```

Expected: build succeeds. The first run takes 15–30 minutes compiling dependencies. If the `haskell:9.8.4` tag does not exist, check https://hub.docker.com/_/haskell/tags for the nearest 9.8.x tag and use that — the snapshot's GHC is 9.8.4, so a mismatch will cause Stack to try to download its own GHC and the `--system-ghc` flag will fail.

- [ ] **Step 4: Verify the container runs and reaches a database**

Point it at the local Postgres. On Linux use `--network host`; on macOS or Windows use `host.docker.internal`:

```bash
docker run --rm -p 10000:10000 \
  -e DATABASE_URL="postgresql://postgres:postgres@host.docker.internal:5432/flashcards" \
  -e JWT_SECRET="local-dev-secret" \
  -e ALLOWED_ORIGINS="http://localhost:3000" \
  flash-review-backend
```

Expected output includes `Connected to PostgreSQL successfully!`, `Schema setup complete.`, and `Listening on port 10000`.

Confirm it serves traffic — an unauthenticated request must be rejected, which proves both the server and its auth are wired:

```bash
curl -i http://localhost:10000/cards
```

Expected: `HTTP/1.1 401 Unauthorized`.

- [ ] **Step 5: Commit**

```bash
git add flash-review-backend/Dockerfile flash-review-backend/.dockerignore
git commit -m "build: add multi-stage Dockerfile for the backend

Runtime stage includes libpq5, which postgresql-simple needs at runtime and
which the reference deployment guide's template omits."
```

---

## Task 7: Declare both services in `render.yaml`

**Files:**
- Create: `render.yaml` (repository root)

**Interfaces:**
- Consumes: the Dockerfile from Task 6; the env var names from Tasks 1, 4, and 5; the frontend build from Tasks 8-10.
- Produces: the deployment definition. Nothing in code depends on it.

- [ ] **Step 1: Create `render.yaml`**

```yaml
services:
  - type: web
    name: flash-review-backend
    runtime: docker
    plan: free
    rootDir: flash-review-backend
    dockerfilePath: ./Dockerfile
    envVars:
      # Neon pooled connection string. Set by hand in the dashboard.
      - key: DATABASE_URL
        sync: false
      # Render generates a random value once and keeps it stable across
      # deploys, which is exactly what JWT session continuity requires.
      - key: JWT_SECRET
        generateValue: true
      # The static site's URL. Only known after its first deploy.
      - key: ALLOWED_ORIGINS
        sync: false

  - type: web
    name: flash-review-frontend
    runtime: static
    rootDir: flash-review-frontend
    # PureScript has no compile-time env var mechanism, so the backend URL is
    # substituted into the source before the build.
    buildCommand: >-
      npm install &&
      sed -i "s|http://localhost:8081|$API_BASE_URL|" src/API/Client.purs &&
      npm run build &&
      npm run bundle
    staticPublishPath: .
    envVars:
      - key: API_BASE_URL
        sync: false
```

No `healthCheckPath` is set. Every endpoint on this API requires authentication and would answer a health probe with 401, which Render reads as unhealthy. Without the setting Render just checks that the process binds its port.

- [ ] **Step 2: Validate the YAML parses**

```bash
python -c "import yaml,sys; yaml.safe_load(open('render.yaml')); print('ok')"
```

Expected: `ok`.

- [ ] **Step 3: Commit**

```bash
git add render.yaml
git commit -m "build: declare backend and frontend services in render.yaml"
```

---

## Task 8: Add a localStorage wrapper to the frontend

**Files:**
- Create: `flash-review-frontend/src/API/Storage.purs`
- Create: `flash-review-frontend/src/API/Storage.js`

**Interfaces:**
- Consumes: nothing.
- Produces: module `API.Storage` exporting `getToken :: Effect (Maybe String)`, `setToken :: String -> Effect Unit`, `clearToken :: Effect Unit`. Tasks 9 and 10 use all three.

The storage key is `frToken` and must match exactly across the FFI and any consumer.

- [ ] **Step 1: Create the FFI implementation**

`src/API/Storage.js`:

```javascript
"use strict";

const KEY = "frToken";

export const setTokenImpl = (token) => () => {
  window.localStorage.setItem(KEY, token);
};

export const getTokenImpl = (nothing) => (just) => () => {
  const token = window.localStorage.getItem(KEY);
  return token === null || token === "" ? nothing : just(token);
};

export const clearTokenImpl = () => {
  window.localStorage.removeItem(KEY);
};
```

- [ ] **Step 2: Create the PureScript wrapper**

`src/API/Storage.purs`:

```purescript
-- | Persists the session JWT in localStorage so a page refresh does not log
-- | the user out. Readable by JS, which is the accepted tradeoff for a SPA on
-- | static hosting.
module API.Storage
  ( getToken
  , setToken
  , clearToken
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import setTokenImpl :: String -> Effect Unit
foreign import getTokenImpl :: Maybe String -> (String -> Maybe String) -> Effect (Maybe String)
foreign import clearTokenImpl :: Effect Unit

setToken :: String -> Effect Unit
setToken = setTokenImpl

getToken :: Effect (Maybe String)
getToken = getTokenImpl Nothing Just

clearToken :: Effect Unit
clearToken = clearTokenImpl
```

- [ ] **Step 3: Verify it compiles**

```bash
cd flash-review-frontend
npm run build
```

Expected: compiles with no errors. `effect` and `maybe` are already in `spago.yaml`'s dependency list, so no manifest change is needed.

- [ ] **Step 4: Commit**

```bash
git add flash-review-frontend/src/API/Storage.purs flash-review-frontend/src/API/Storage.js
git commit -m "feat: add a localStorage wrapper for the session token"
```

---

## Task 9: Send bearer tokens instead of cookies

Every request in `Client.purs` currently sends `credentials: Include`. With the frontend and backend on different Render domains that cookie is cross-site and browsers increasingly refuse it.

There is a second defect to fix here. `/login` is declared `Post '[JSON] String` (`API.hs:75`), so servant returns the JWT **JSON-encoded** — the response body is `"eyJhbGci..."` including the surrounding quotes. The current `login` returns `response.text` raw, which nobody consumed, so it never mattered. Stored verbatim as a bearer token, those quotes would make every authenticated request fail. Decoding through the existing `handleJsonResponse` strips them.

**Files:**
- Modify: `flash-review-frontend/src/API/Client.purs` (all 9 request functions)

**Interfaces:**
- Consumes: `API.Storage.getToken`, `API.Storage.setToken` (Task 8).
- Produces: `Client.login` now persists the token as a side effect. Task 10 relies on that.

- [ ] **Step 1: Replace the imports and add header helpers**

At the top of `src/API/Client.purs`, replace the import block and `baseUrl` with:

```purescript
module API.Client where

import Prelude

import API.Storage as Storage
import API.Types (Flashcard(..), ReviewResult, Stats, User, UserCredentials)
import API.UUID (SerializableUUID)
import API.UUID (unwrap) as UUID
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Data.UUID (toString) as UUID
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Fetch (fetch, Method(..), Response)

-- Replaced at build time by Render's build command, which substitutes
-- $API_BASE_URL for this literal before spago compiles the project.
baseUrl :: String
baseUrl = "http://localhost:8081"
```

`RequestCredentials(..)` is dropped from the `Fetch` import because `credentials: Include` is gone everywhere.

Add the two header helpers after `handleJsonResponse`:

```purescript
-- | The header is always sent, empty when no token is stored. The backend
-- | treats an unparseable token as unauthenticated and answers 401, which is
-- | the behavior we want for a logged-out client.
authHeaders :: Aff { "Authorization" :: String }
authHeaders = do
  mToken <- liftEffect Storage.getToken
  pure { "Authorization": "Bearer " <> fromMaybe "" mToken }

authJsonHeaders :: Aff { "Authorization" :: String, "Content-Type" :: String }
authJsonHeaders = do
  mToken <- liftEffect Storage.getToken
  pure
    { "Authorization": "Bearer " <> fromMaybe "" mToken
    , "Content-Type": "application/json"
    }
```

- [ ] **Step 2: Rewrite the seven authenticated request functions**

```purescript
getAllCards :: Aff (Either String (Array Flashcard))
getAllCards = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/cards") { headers }
  handleJsonResponse decodeJson response

createCard :: Flashcard -> Aff (Either String Flashcard)
createCard card = do
  headers <- authJsonHeaders
  let opts =
        { method: POST
        , headers
        , body: toJsonString card
        }
  response <- fetch (baseUrl <> "/cards") opts
  handleJsonResponse decodeJson response

updateCard :: Flashcard -> Aff (Either String Flashcard)
updateCard card@(Flashcard c) = do
  headers <- authJsonHeaders
  let idString = UUID.toString (UUID.unwrap c.id)
      opts =
        { method: PUT
        , headers
        , body: toJsonString card
        }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  handleJsonResponse decodeJson response

deleteCard :: SerializableUUID -> Aff (Either String Unit)
deleteCard id = do
  headers <- authHeaders
  let idString = UUID.toString (UUID.unwrap id)
      opts = { method: DELETE, headers }
  response <- fetch (baseUrl <> "/cards/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "DELETE /cards/" <> idString <> " request failed with status: " <> show response.status

getReviewQueue :: Aff (Either String (Array Flashcard))
getReviewQueue = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/review/queue") { headers }
  handleJsonResponse decodeJson response

submitReview :: SerializableUUID -> ReviewResult -> Aff (Either String Unit)
submitReview id result = do
  headers <- authJsonHeaders
  let idString = UUID.toString (UUID.unwrap id)
      opts =
        { method: POST
        , headers
        , body: toJsonString result
        }
  response <- fetch (baseUrl <> "/review/" <> idString) opts
  if response.ok
    then pure $ Right unit
    else pure $ Left $ "POST /review/" <> idString <> " request failed with status: " <> show response.status

getStats :: Aff (Either String Stats)
getStats = do
  headers <- authHeaders
  response <- fetch (baseUrl <> "/stats") { headers }
  handleJsonResponse decodeJson response
```

- [ ] **Step 3: Rewrite `login` and `signup`**

`login` decodes through `handleJsonResponse` to strip servant's JSON quoting, then persists the token. `signup` simply drops `credentials`.

```purescript
login :: UserCredentials -> Aff (Either String String)
login credentials = do
  let opts =
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        }
  response <- fetch (baseUrl <> "/login") opts
  -- /login is declared `Post '[JSON] String`, so the body arrives JSON-encoded
  -- with surrounding quotes. Decoding strips them; storing the raw text would
  -- produce a bearer token the backend cannot parse.
  result <- handleJsonResponse decodeJson response
  case result of
    Left err -> pure $ Left $ "Login failed: " <> err
    Right token -> do
      liftEffect $ Storage.setToken token
      pure $ Right token

signup :: UserCredentials -> Aff (Either String User)
signup credentials = do
  let opts =
        { method: POST
        , headers: { "Content-Type": "application/json" }
        , body: toJsonString credentials
        }
  response <- fetch (baseUrl <> "/signup") opts
  handleJsonResponse decodeJson response
```

- [ ] **Step 4: Verify it compiles**

```bash
cd flash-review-frontend
npm run build
```

Expected: compiles with no errors. If `fetch` rejects a record with only `headers` and no `method`, supply `method: GET` explicitly on the three GET calls.

- [ ] **Step 5: Commit**

```bash
git add flash-review-frontend/src/API/Client.purs
git commit -m "feat: authenticate with bearer tokens instead of cookies

Frontend and backend sit on different Render domains, making the session
cookie cross-site. Also decodes /login's response as JSON: servant returns
the JWT quoted, and storing it raw yields an unparseable bearer token."
```

---

## Task 10: Restore the session on page load

`App`'s initial state hardcodes `isLoggedIn: false`, so a refresh drops the user at the login screen even with a valid stored token. `Login` also discards the token that `Client.login` now persists.

**Files:**
- Modify: `flash-review-frontend/src/Main.purs`
- Modify: `flash-review-frontend/src/Components/App.purs:50-59,206`

**Interfaces:**
- Consumes: `API.Storage.getToken`, `API.Storage.clearToken` (Task 8); `Client.login`'s persistence (Task 9).
- Produces: `App.component :: H.Component Query Boolean output Aff` — its input is now whether a token was found, not `Unit`.

`Login` needs no change: `Client.login` already stores the token, and `LoginSuccessful` continues to carry no payload.

- [ ] **Step 1: Read the stored token at startup**

Replace `src/Main.purs`:

```purescript
module Main where

import Prelude

import API.Storage as Storage
import Components.App (component)
import Data.Maybe (isJust)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  hasToken <- isJust <$> Storage.getToken
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component hasToken body
```

- [ ] **Step 2: Accept the flag as component input**

In `src/Components/App.purs`, replace the component definition at lines 50-59:

```purescript
component :: forall output. H.Component Query Boolean output Aff
component =
  H.mkComponent
    { initialState: \hasToken ->
        { currentView: if hasToken then FlashcardsView else LoginView
        , isLoggedIn: hasToken
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
```

The `forall input` type variable is replaced by the concrete `Boolean` input.

- [ ] **Step 3: Clear the token on logout**

At `src/Components/App.purs:206`, the `Logout` handler currently only resets state. Replace it with:

```purescript
  Logout -> do
    H.liftEffect Storage.clearToken
    H.modify_ \st -> st { isLoggedIn = false, currentView = LoginView }
```

Add the import:

```purescript
import API.Storage as Storage
```

- [ ] **Step 4: Verify it compiles and bundles**

```bash
cd flash-review-frontend
npm run build && npm run bundle
```

Expected: both succeed, producing `index.js`.

- [ ] **Step 5: Verify the full flow end to end locally**

Start the backend and the frontend in separate terminals. Postgres is the native service and is already running — do not start a container (see Environment):

```powershell
$env:PATH = "C:\ghcup\bin;$env:PATH"
cd flash-review-backend
$env:ALLOWED_ORIGINS="http://localhost:3000"
$env:JWT_SECRET="local-dev-secret"
stack exec flash-review-backend-exe
```

```bash
cd flash-review-frontend && npm run serve
```

In a browser at `http://localhost:3000`, confirm each of these:

1. Sign up a new user, then log in — the flashcards view appears.
2. In DevTools → Application → Local Storage, `frToken` holds a JWT with **no** surrounding quotes. Quotes mean Task 9 Step 3 did not take effect.
3. In DevTools → Network, a request to `/cards` carries an `Authorization: Bearer …` header and **no** `Cookie` header.
4. Create a flashcard; it appears in the list.
5. Refresh the page — still logged in, cards still listed.
6. Click Logout — returns to login, and `frToken` is gone from Local Storage.

- [ ] **Step 6: Commit**

```bash
git add flash-review-frontend/src/Main.purs flash-review-frontend/src/Components/App.purs
git commit -m "feat: restore the session from localStorage on page load

App's input is now whether a stored token was found, so a refresh no longer
drops the user at the login screen. Logout clears the token."
```

---

## Task 11: Deploy

No code changes. This task is the deployment itself and the post-deploy verification.

**Files:** none.

- [ ] **Step 1: Create the Neon database**

1. Sign up at https://neon.tech (no card required).
2. Create a project; note the database name.
3. Click **Connect** and copy the connection string.
4. Use the **pooled** string — its hostname contains `-pooler`. Neon's free tier has a low direct-connection limit and the app opens up to 5.

- [ ] **Step 2: Push the branch and deploy the backend**

```bash
git push -u origin render-neon-deploy
```

On render.com: **New → Web Service**, connect the repository, select this branch.

- Root Directory: `flash-review-backend`
- Runtime: Docker
- Instance Type: Free

Environment variables:

| Key | Value |
|---|---|
| `DATABASE_URL` | the Neon **pooled** connection string |
| `JWT_SECRET` | a long random string (`openssl rand -base64 32`) |
| `ALLOWED_ORIGINS` | `https://placeholder.invalid` for now |

Deploy. Expect 15–30 minutes for the first build. Watch the logs for `Connected to PostgreSQL successfully!`, `Schema setup complete.`, and `Listening on port`.

- [ ] **Step 3: Confirm the backend is live**

```bash
curl -i https://<your-backend>.onrender.com/cards
```

Expected: `HTTP/1.1 401 Unauthorized`. A 401 here is success — it proves the server is up, reached the database during startup, and is enforcing auth.

- [ ] **Step 4: Deploy the frontend**

**New → Static Site**, same repository and branch.

- Root Directory: `flash-review-frontend`
- Build Command:
  ```
  npm install && sed -i "s|http://localhost:8081|$API_BASE_URL|" src/API/Client.purs && npm run build && npm run bundle
  ```
- Publish Directory: `.`
- Environment variable: `API_BASE_URL` = `https://<your-backend>.onrender.com` (no trailing slash)

- [ ] **Step 5: Point the backend's CORS at the frontend**

Set the backend's `ALLOWED_ORIGINS` to the static site's URL (for example `https://flash-review-frontend.onrender.com`, no trailing slash) and redeploy the backend.

- [ ] **Step 6: Verify the deployment**

Against the live frontend URL:

1. Sign up, then log in.
2. Create a flashcard.
3. Refresh the page — session persists.
4. **Wait 15+ minutes without touching the app**, then reload and make a request.

Step 4 is the one that matters most and the one most likely to be skipped. It is the only check that exercises both risky changes at once: the pool reconnecting to a Neon instance that has scaled to zero, and `JWT_SECRET` surviving a Render cold start. A token minted before the shutdown must still be accepted afterward. If you are logged out, `JWT_SECRET` is not stable. If you get a 500, the pool is not recycling dead connections.

- [ ] **Step 7: Open a pull request**

```bash
gh pr create --title "Deploy to Render + Neon" --body "$(cat <<'EOF'
## Summary
- Bearer-token auth replacing cross-site cookies
- Connection pool replacing the single long-lived PG connection
- JWT signing key from JWT_SECRET so cold starts do not invalidate sessions
- DATABASE_URL support with the PG* path retained for local dev and CI
- PORT and ALLOWED_ORIGINS from the environment
- Multi-stage Dockerfile and render.yaml

Also revives the test suite: Spec.hs never imported MainSpec, so APISpec,
DatabaseSpec, and ServerSpec were compiled but never executed. Running them
surfaced a JWT key mismatch in ServerSpec and a missing updateCardDb call in
DatabaseSpec, both fixed.

## Test plan
- `stack test` green
- Local end-to-end: signup, login, create card, refresh, logout
- Deployed: same flow, plus a 15-minute idle check confirming pool reconnect
  and JWT stability across a cold start

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Deviations from the spec

Recorded so review can catch anything unintended:

1. **New `Config` module.** The spec put `PORT`/`ALLOWED_ORIGINS` parsing in `Main.hs`. `Main.hs` is an executable and cannot be imported by the test suite, so parsing moved to `src/Config.hs` to be testable. `Main.hs` just calls it.
2. **Dockerfile binary path.** The spec described `stack path --local-install-root`; the plan uses `stack build --copy-bins --local-bin-path /app/bin`, which puts the binary at a fixed known path. Same outcome, simpler.
3. **`/login` JSON quoting.** Not in the spec — found while writing Task 9. Servant returns the JWT JSON-encoded, so the raw response body carries surrounding quotes. Storing it verbatim would break every authenticated request. Handled by decoding through the existing `handleJsonResponse`.
4. **No `healthCheckPath` in `render.yaml`.** Every endpoint requires auth and would answer a probe with 401, which Render reads as unhealthy.
