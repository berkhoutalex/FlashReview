# Deploying FlashReview on Render + Neon

**Date:** 2026-07-18
**Status:** Approved

## Goal

Host the existing Haskell backend and PureScript frontend on Render's free tier, with
Postgres on Neon's free tier, at $0/month.

## Context

The app currently runs only locally: the backend binds a hardcoded port, authenticates
with cookies against a hardcoded `localhost:3000` origin, and connects to a local
Postgres via `PGHOST`/`PGUSER`/etc. Four properties of the current code are incompatible
with the target hosting model, and each drives a change below.

| # | Current behavior | Why it breaks on Render + Neon |
|---|---|---|
| 1 | Cookie auth, `credentials: Include` (`Client.purs`) | Frontend and backend land on different `*.onrender.com` domains, making the cookie cross-site. Safari blocks these today. |
| 2 | JWT key from `generateKey` at boot (`Server.hs:34`) | Render free tier spins down after 15 min idle; every cold start invalidates all outstanding tokens. |
| 3 | One `PG.Connection` held for process lifetime (`Server.hs:24`) | Neon scales compute to zero and drops idle connections. No pool, no reconnect: first request after idle 500s and stays broken. |
| 4 | Five separate `PG*` env vars, no TLS (`Database.hs:77-81`) | Neon provides a single `DATABASE_URL` and requires `sslmode=require`. |

## Decisions

Recorded with their rejected alternatives, since each was a genuine fork.

**Auth: bearer tokens, not cookies.** Rejected serving the frontend from the backend
(same-origin, but the frontend would then cold-start too) and patching cookies to
`SameSite=None; Secure` (smallest diff, but Safari blocks it now and Chrome is
following). `POST /login` already returns the raw JWT in its body — the frontend simply
stops discarding it.

**DB: `resource-pool`, not single-connection reconnect.** `resource-pool` is already a
declared dependency in `package.yaml` and currently unused. Rejected an IORef-based
reconnect-and-retry wrapper: smaller diff, but it serializes all DB work through one
connection and retry-once logic risks replaying writes.

**Token storage: `localStorage`.** Rejected in-memory-only: it logs the user out on every
page refresh, which is poor for an app you dip in and out of. `localStorage` is
XSS-readable; that is the accepted, standard tradeoff for a SPA on static hosting and is
the same exposure a non-`HttpOnly` cookie would carry.

**Schema: keep `setupSchema` on boot.** All statements are idempotent `IF NOT EXISTS`
DDL, so a fresh Neon database works with zero manual steps. Rejected the guide's
one-time-manual-migration advice: it buys a marginally faster cold start in exchange for
a step that must be remembered for every new environment.

## Architecture

| Piece | Render service | Root directory | Cold start |
|---|---|---|---|
| Frontend | Static Site | `flash-review-frontend` | None |
| Backend | Web Service (Docker), free plan | `flash-review-backend` | After 15 min idle |
| Database | — (Neon) | — | ~500ms resume from zero |

The monorepo layout is unchanged. Both Render services point at the same repository and
are distinguished by their Root Directory setting.

## Backend changes

### `src/Database.hs`

Add `DATABASE_URL` support with the existing path as fallback:

- If `DATABASE_URL` is set, pass it to `PG.connectPostgreSQL` verbatim. The Neon string
  already carries `sslmode=require`.
- Otherwise fall back to the current `PGHOST`/`PGPORT`/`PGUSER`/`PGPASSWORD`/`PGDATABASE`
  assembly, keeping local dev, `docker-compose.yml`, and the `backend-tests.yml` CI
  workflow working without modification.

Add `mkPool :: IO (Pool PG.Connection)`: max 5 connections, 30s idle timeout. Both sit
comfortably inside Neon free-tier limits, and the idle timeout retires connections before
Neon drops them.

### `src/Server.hs`

- `AppEnv.appDbConn :: PG.Connection` becomes `appDbPool :: Pool PG.Connection`.
- All 9 handlers wrap their DB call in `withResource appDbPool $ \conn -> ...`. This is
  mechanical: every function in `Database.hs` already takes a `Connection`, so
  `Database.hs` signatures are untouched.
- `initializeApp` reads `JWT_SECRET` and derives the key with `fromSecret` rather than
  `generateKey`. If the variable is absent it falls back to `generateKey` and logs a
  warning, so local dev is unaffected while production gets sessions stable across cold
  starts.
- `setupSchema` runs via `withResource` against the pool.

Cookie machinery (`acceptLogin`, the `Set-Cookie` response headers on `/login`) is left
in place. It becomes inert under bearer auth, and removing it would require changing
`API.hs`'s endpoint type and its tests. Tracked as optional follow-up cleanup, explicitly
out of scope here.

### `app/Main.hs`

- Read the port from `PORT` (Render sets it), defaulting to 8081.
- Read CORS origins from `ALLOWED_ORIGINS` (comma-separated), defaulting to
  `http://localhost:3000`. `corsRequestHeaders` already includes `hAuthorization`, which
  is what permits bearer tokens cross-origin — keep it.
- Simplify the API proxy from `'[JWT, Cookie]` to `'[JWT]`.

### `test/ServerSpec.hs`

Line 75 constructs `AppEnv` directly; update it to build a pool.

### New: `flash-review-backend/Dockerfile`

Multi-stage, stack-based rather than the reference guide's cabal version, since the
project uses `stack.yaml` pinned to `lts-23.26` (GHC 9.8.4).

Two corrections to the reference guide's template:

- The runtime stage must install **`libpq5`**. `postgresql-simple` links against libpq;
  the guide's runtime image omits it and the server would fail at startup.
- The build stage needs `libpq-dev` and `zlib1g-dev`.

The built binary is located via `stack path --local-install-root` rather than the guide's
hardcoded `dist-newstyle` path.

### New: `render.yaml`

Blueprint at the repository root declaring both services, so the deployment is
reproducible rather than hand-configured through the dashboard.

## Frontend changes

### New: `src/API/Storage.purs` + `Storage.js`

Thin FFI wrapper over `localStorage`: `getToken`, `setToken`, `clearToken`.

### `src/API/Client.purs`

- `baseUrl` stays a plain compile-time constant defaulting to `http://localhost:8081`, and
  the Render build command substitutes it before compiling (see below).
- All 9 request functions drop `credentials: Include` and instead read the token from
  storage and send `Authorization: Bearer <token>`. Reading inside `Client` rather than
  threading a token parameter through every call site leaves the component layer's
  function signatures unchanged.
- The header is always sent, empty when no token is stored; the backend treats an
  unparseable token as unauthenticated and returns 401, which is the desired behavior.
- `login` stores the returned token before returning.

### Injecting the backend URL at build time

PureScript has no compile-time environment-variable mechanism, so the value must be
supplied externally. Two options were considered:

1. **Source substitution.** The Render build command rewrites the `baseUrl` literal in
   `Client.purs` from the `API_BASE_URL` environment variable before `spago build` runs.
2. **Runtime FFI.** `baseUrl` becomes an `Effect String` reading a JS global that
   `index.html` sets, with the global's value substituted at build time.

**Chosen: source substitution.** Option 2 changes `baseUrl`'s type from `String` to
`Effect String`, which ripples through every call site in `Client.purs`, and still needs
a build-time substitution step — into `index.html` instead of `Client.purs`. It adds a
moving part without removing one. Substitution is crude but keeps `baseUrl` a pure
constant and touches nothing beyond the build command. The local default remains valid,
so a developer who never sets `API_BASE_URL` sees no change.

### `src/Components/Login.purs`, `src/Components/App.purs`

- `Login.LoginSuccessful` currently carries no payload and the token is discarded; login
  now persists it via `Storage`.
- `App`'s initial state checks for a stored token so a page refresh restores the session
  rather than bouncing to `LoginView`.
- Logout (`App.purs:206`) clears stored token.

## Deploy sequence

1. Create Neon project; copy the **pooled** connection string (hostname contains
   `-pooler`).
2. Render → New Web Service. Root directory `flash-review-backend`, runtime Docker,
   instance type Free. Environment: `DATABASE_URL`, `JWT_SECRET` (a long random value),
   `ALLOWED_ORIGINS` (placeholder initially).
3. Render → New Static Site. Root directory `flash-review-frontend`, publish directory
   `.`, environment `API_BASE_URL` set to the backend's URL. Build command substitutes
   that URL into the source before compiling:

   ```sh
   npm install \
     && sed -i "s|http://localhost:8081|$API_BASE_URL|" src/API/Client.purs \
     && npm run build \
     && npm run bundle
   ```
4. Set the backend's `ALLOWED_ORIGINS` to the static site's URL; redeploy the backend.

The first backend build takes 15–30 minutes — Haskell compiling from scratch with no
build cache. Later builds are faster only when Docker layer caching hits.

## Testing

### Pre-existing condition: the HTTP test suite is dormant

Discovered while planning. `test/Spec.hs` is the test entrypoint and never imports
`MainSpec` — the module that aggregates `APISpec`, `DatabaseSpec`, and `ServerSpec`.
`Spec.hs` instead re-implements roughly five tests inline. What CI runs today is those
five tests; the eight HTTP-level tests in `ServerSpec.hs` are compiled but never
executed.

Two dormant defects are visible by inspection and will surface once the suite is wired
up:

- `ServerSpec.hs` calls `generateKey` twice independently — line 68 in `makeTestApp` and
  line 95 in `getTestToken`. Tokens are therefore signed with one key and validated
  against another, so every authenticated test asserting 200 should receive 401.
- `DatabaseSpec.hs:100-111` ("should update a flashcard") never calls `updateCardDb`. It
  creates a card, constructs an `updatedCard` value, then asserts the database returns
  the updated version.

These are predictions from reading the code, not observed failures — the suite has not
been run. Neither is caused by this migration, but both sit in code the migration
changes.

### Approach

Reviving the suite is a **prerequisite task**, before any migration work. The connection
pool and `JWT_SECRET` changes are precisely what these tests would catch, and TDD on them
is meaningless while the suite does not execute. The `JWT_SECRET` change also makes the
shared-key fix natural, since both `makeTestApp` and `getTestToken` will derive their key
from one secret.

Replacing `Spec.hs`'s inline tests with a delegation to `MainSpec` loses no coverage:
`DatabaseSpec` strictly supersedes the inline database tests, and `APISpec` supersedes the
inline serialization test.

The `DATABASE_URL` fallback preserves the `PG*` path that CI uses, so `backend-tests.yml`
needs no change.

Post-deploy verification is manual, in order:

1. Sign up, then log in.
2. Create a flashcard.
3. Refresh the page — session persists (validates `localStorage` restore).
4. Wait 15+ minutes for the backend to spin down, then make a request — it returns
   successfully (validates both pool reconnect against a scaled-to-zero Neon instance and
   `JWT_SECRET` surviving the restart, since the pre-existing token must still be
   accepted).

## Known limitations

- **Static site publish directory is `.`**, so Render serves the whole frontend directory
  including `src/` and `spago.yaml`. Harmless — it is public source regardless — but
  untidy. Assembling a real `dist/` via an esbuild output step is deliberately out of
  scope.
- **`server.js` becomes dead code in production**; Render's static hosting replaces it.
  Retained because it remains useful for local development.
- **Backend cold starts** of a few seconds after 15 minutes idle are accepted. Render's
  $7/mo Starter plan removes the spin-down when that becomes unacceptable.
