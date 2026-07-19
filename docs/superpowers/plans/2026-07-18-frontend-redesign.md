# FlashReview Frontend Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Redesign the `flash-review-frontend` PureScript/Halogen UI into a clean, minimal, dark-first SaaS look with a collapsible left sidebar, indigo accent, and a light-mode toggle — replacing today's inline `halogen-css` styling with split CSS files, without changing any API/business logic.

**Architecture:** Three new CSS files (`tokens.css`, `layout.css`, `components.css`) linked from `index.html`, applied via `HP.class_` on existing Halogen components (replacing `HCSS.style` blocks). A new `Theme` FFI module manages the `data-theme` attribute + `localStorage` persistence for the dark/light toggle. `Components/App.purs` becomes a sidebar shell instead of a top-header shell. Every other component (`Login`, `Signup`, `FlashcardForm`, `FlashcardList`, `Review`, `Stats`) is restyled to use the new class-based system.

**Tech Stack:** PureScript 0.15.15, Halogen, spago 0.93.44, esbuild (via `spago bundle`), plain CSS (no preprocessor, no bundler-level CSS pipeline), Express static file server (`server.js`, unchanged).

**Spec:** `docs/superpowers/specs/2026-07-18-frontend-redesign-design.md`

## Global Constraints

- No new spago/npm dependencies (no icon library, no webfont, no CSS framework). `spago.yaml` and `package.json` are not modified.
- Typography: system font stack only — `-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Inter, Helvetica, Arial, sans-serif`.
- Theme: dark is the default (`:root` in `tokens.css`); light is an opt-in override via `[data-theme="light"]`, toggled by the user and persisted in `localStorage` under the key `flashreview-theme`.
- Accent color: indigo — dark `#7c9eff` / light `#5b7cfa`.
- Responsive breakpoint: sidebar collapses to a top bar + slide-in drawer at `max-width: 768px`.
- Icons: dependency-free Unicode glyphs (e.g. `☰`, `⚡`, `🗑`), not an SVG/icon-font library. This is a deliberate simplification of the spec's "small inline SVG icons" language — Halogen has no ergonomic built-in SVG element DSL without adding a new package, and Unicode glyphs achieve the same "icon + label" sidebar look with zero new dependencies and zero risk of malformed markup.
- No new automated tests are added (per spec's Testing & Verification section) — this is a presentation-layer-only change. Every task's verification step is `npm run build` (compiler as correctness gate) plus, for user-visible tasks, a manual visual check via `npm run bundle && npm run serve`.
- All work happens on the `frontend-redesign` branch (already checked out) — never commit to `main`.
- **Shell note (this machine):** run `npm run build` / `npm run bundle` / `npm run serve` via **PowerShell**, not the Bash/Git-Bash tool. Git Bash's MSYS2 process spawning causes `spago`'s `purs.cmd` invocation to fail with `EINVAL` (a very long, glob-heavy command line trips something in how Git Bash mediates the spawn) even though the exact same command succeeds cleanly in PowerShell. Confirmed both `npm run build` and `npm run bundle` succeed via PowerShell with zero errors/warnings.

---

## File Structure

**New files:**
- `flash-review-frontend/public/css/tokens.css` — design tokens (CSS custom properties), dark default + light override.
- `flash-review-frontend/public/css/layout.css` — global reset, app shell (sidebar/topbar/drawer), auth shell, responsive breakpoint.
- `flash-review-frontend/public/css/components.css` — buttons, inputs, cards, nav links, alerts, badges, flashcard grid/tiles, review card, stat tile.
- `flash-review-frontend/src/Theme.purs` — FFI wrapper: `initTheme`, `setTheme`.
- `flash-review-frontend/src/Theme.js` — FFI implementation (DOM attribute + `localStorage`).

**Modified files:**
- `flash-review-frontend/index.html` — link the three stylesheets, drop the inline `<style>` block.
- `flash-review-frontend/src/Main.purs` — call `Theme.initTheme` before mounting, pass the result as `App`'s input.
- `flash-review-frontend/src/Components/App.purs` — sidebar shell, mobile drawer, theme toggle.
- `flash-review-frontend/src/Components/Login.purs` — class-based restyle.
- `flash-review-frontend/src/Components/Signup.purs` — class-based restyle.
- `flash-review-frontend/src/Components/FlashcardForm.purs` — class-based restyle.
- `flash-review-frontend/src/Components/FlashcardList.purs` — class-based restyle, grid layout.
- `flash-review-frontend/src/Components/Review.purs` — class-based restyle, progress indicator, difficulty-colored ratings.
- `flash-review-frontend/src/Components/Stats.purs` — class-based restyle, stat tile.

---

### Task 1: CSS foundation (tokens, layout, components) + wire into index.html

**Files:**
- Create: `flash-review-frontend/public/css/tokens.css`
- Create: `flash-review-frontend/public/css/layout.css`
- Create: `flash-review-frontend/public/css/components.css`
- Modify: `flash-review-frontend/index.html`

**Interfaces:**
- Produces: every CSS custom property and class name referenced by later tasks (listed below). Later tasks consume these by exact name — do not rename without updating this file.

- [ ] **Step 1: Create `public/css/tokens.css`**

```css
:root {
  /* Surfaces */
  --bg: #0b0e14;
  --surface: #11151d;
  --surface-raised: #151a24;
  --border: #1f2530;

  /* Text */
  --text: #e6e9ef;
  --text-muted: #8b93a5;
  --text-faint: #5b6272;

  /* Accent (indigo) */
  --accent: #7c9eff;
  --accent-hover: #95afff;
  --accent-muted: #2a3142;
  --accent-contrast: #0b0e14;

  /* Semantic */
  --danger: #f4534a;
  --danger-bg: #3a1f1f;
  --success: #4ade80;
  --success-bg: #1c3324;

  /* Spacing */
  --space-1: 4px;
  --space-2: 8px;
  --space-3: 12px;
  --space-4: 16px;
  --space-5: 24px;
  --space-6: 32px;
  --space-7: 40px;
  --space-8: 48px;

  /* Type */
  --font-sans: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Inter, Helvetica, Arial, sans-serif;
  --text-xs: 12px;
  --text-sm: 13px;
  --text-base: 15px;
  --text-lg: 17px;
  --text-xl: 20px;
  --text-2xl: 28px;
  --font-weight-normal: 400;
  --font-weight-medium: 500;
  --font-weight-bold: 700;

  /* Radii & shadows */
  --radius-sm: 6px;
  --radius-md: 10px;
  --radius-lg: 14px;
  --shadow-card: 0 1px 2px rgba(0, 0, 0, 0.4);

  /* Motion */
  --transition-fast: 120ms ease;

  /* Layout */
  --sidebar-width: 220px;
}

[data-theme="light"] {
  --bg: #f7f8fa;
  --surface: #ffffff;
  --surface-raised: #ffffff;
  --border: #e4e7ec;

  --text: #1a1d24;
  --text-muted: #5b6272;
  --text-faint: #8b93a5;

  --accent: #5b7cfa;
  --accent-hover: #4a68e0;
  --accent-muted: #e8ecff;
  --accent-contrast: #ffffff;

  --danger: #d92d20;
  --danger-bg: #fdeceb;
  --success: #1a9d5c;
  --success-bg: #e7f8ee;

  --shadow-card: 0 1px 3px rgba(16, 24, 40, 0.1), 0 1px 2px rgba(16, 24, 40, 0.06);
}
```

- [ ] **Step 2: Create `public/css/layout.css`**

```css
*, *::before, *::after {
  box-sizing: border-box;
}

html, body {
  margin: 0;
  padding: 0;
  height: 100%;
}

body {
  background: var(--bg);
  color: var(--text);
  font-family: var(--font-sans);
  font-size: var(--text-base);
  line-height: 1.5;
  transition: background-color var(--transition-fast), color var(--transition-fast);
}

a {
  color: inherit;
}

button, input, textarea {
  font-family: inherit;
  font-size: inherit;
}

/* App shell (logged-in) */
.app-shell {
  display: flex;
  min-height: 100vh;
}

.sidebar {
  width: var(--sidebar-width);
  flex-shrink: 0;
  background: var(--surface);
  border-right: 1px solid var(--border);
  display: flex;
  flex-direction: column;
  padding: var(--space-4) var(--space-3);
}

.sidebar-wordmark {
  font-size: var(--text-lg);
  font-weight: var(--font-weight-bold);
  color: var(--text);
  padding: var(--space-2) var(--space-2) var(--space-5);
}

.sidebar-nav {
  display: flex;
  flex-direction: column;
  gap: var(--space-1);
}

.sidebar-footer {
  margin-top: auto;
  display: flex;
  flex-direction: column;
  gap: var(--space-1);
  padding-top: var(--space-4);
  border-top: 1px solid var(--border);
}

.main-content {
  flex: 1;
  min-width: 0;
  padding: var(--space-6);
  overflow-y: auto;
}

/* Mobile topbar + drawer */
.topbar {
  display: none;
}

.drawer-backdrop {
  display: none;
}

@media (max-width: 768px) {
  .app-shell {
    flex-direction: column;
  }

  .sidebar {
    position: fixed;
    top: 0;
    left: 0;
    bottom: 0;
    z-index: 20;
    transform: translateX(-100%);
    transition: transform var(--transition-fast);
    box-shadow: var(--shadow-card);
  }

  .sidebar.is-open {
    transform: translateX(0);
  }

  .topbar {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: var(--space-3) var(--space-4);
    background: var(--surface);
    border-bottom: 1px solid var(--border);
    position: sticky;
    top: 0;
    z-index: 10;
  }

  .topbar .sidebar-wordmark {
    padding: 0;
  }

  .drawer-backdrop.is-open {
    display: block;
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.5);
    z-index: 15;
  }

  .main-content {
    padding: var(--space-4);
  }
}

/* Auth (logged-out) shell */
.auth-shell {
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: var(--space-6);
  padding: var(--space-4);
}

.auth-wordmark {
  font-size: var(--text-xl);
  font-weight: var(--font-weight-bold);
  color: var(--text);
}
```

- [ ] **Step 3: Create `public/css/components.css`**

```css
/* Buttons */
.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: var(--space-2);
  padding: var(--space-3) var(--space-4);
  border-radius: var(--radius-sm);
  border: 1px solid transparent;
  background: transparent;
  font-size: var(--text-base);
  font-weight: var(--font-weight-medium);
  cursor: pointer;
  transition: background-color var(--transition-fast), border-color var(--transition-fast), transform var(--transition-fast), opacity var(--transition-fast);
}

.btn:hover:not(:disabled) {
  transform: translateY(-1px);
}

.btn:disabled {
  cursor: not-allowed;
  opacity: 0.6;
  transform: none;
}

.btn:focus-visible {
  outline: 2px solid var(--accent);
  outline-offset: 2px;
}

.btn-block {
  width: 100%;
}

.btn-primary {
  background: var(--accent);
  color: var(--accent-contrast);
}

.btn-primary:hover:not(:disabled) {
  background: var(--accent-hover);
}

.btn-secondary {
  background: var(--surface-raised);
  color: var(--text);
  border-color: var(--border);
}

.btn-secondary:hover:not(:disabled) {
  border-color: var(--text-faint);
}

.btn-danger-outline {
  color: var(--danger);
  border-color: var(--danger);
}

.btn-danger-outline:hover:not(:disabled) {
  background: var(--danger-bg);
}

.btn-success-outline {
  color: var(--success);
  border-color: var(--success);
}

.btn-success-outline:hover:not(:disabled) {
  background: var(--success-bg);
}

.btn-icon {
  padding: var(--space-2);
  color: var(--text-muted);
}

.btn-icon:hover:not(:disabled) {
  background: var(--surface-raised);
  color: var(--text);
}

/* Inputs */
.field {
  margin-bottom: var(--space-4);
  flex: 1;
}

.label {
  display: block;
  margin-bottom: var(--space-2);
  font-size: var(--text-sm);
  color: var(--text-muted);
}

.input, .textarea {
  display: block;
  width: 100%;
  padding: var(--space-3);
  background: var(--surface-raised);
  border: 1px solid var(--border);
  border-radius: var(--radius-sm);
  color: var(--text);
  transition: border-color var(--transition-fast);
}

.textarea {
  min-height: 100px;
  resize: vertical;
}

.input:focus-visible, .textarea:focus-visible {
  outline: none;
  border-color: var(--accent);
  box-shadow: 0 0 0 3px var(--accent-muted);
}

/* Cards */
.card {
  background: var(--surface-raised);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  box-shadow: var(--shadow-card);
  padding: var(--space-5);
}

.auth-card {
  width: 100%;
  max-width: 360px;
}

.auth-footer {
  text-align: center;
  margin-top: var(--space-4);
  font-size: var(--text-sm);
  color: var(--text-muted);
}

.link {
  color: var(--accent);
  cursor: pointer;
  text-decoration: none;
}

.link:hover {
  text-decoration: underline;
}

/* Nav links */
.nav-link {
  display: flex;
  align-items: center;
  gap: var(--space-3);
  padding: var(--space-3);
  border-radius: var(--radius-sm);
  border: none;
  background: transparent;
  color: var(--text-muted);
  font-size: var(--text-sm);
  font-weight: var(--font-weight-medium);
  cursor: pointer;
  text-decoration: none;
  text-align: left;
  transition: background-color var(--transition-fast), color var(--transition-fast);
}

.nav-link:hover {
  background: var(--bg);
  color: var(--text);
}

.nav-link.is-active {
  background: var(--accent-muted);
  color: var(--accent);
}

/* Hamburger (mobile menu toggle) */
.hamburger {
  background: transparent;
  border: none;
  color: var(--text);
  font-size: var(--text-lg);
  cursor: pointer;
  padding: var(--space-2);
}

/* Alerts */
.alert {
  padding: var(--space-3) var(--space-4);
  border-radius: var(--radius-sm);
  font-size: var(--text-sm);
  margin-bottom: var(--space-4);
}

.alert-error {
  background: var(--danger-bg);
  color: var(--danger);
}

.alert-success {
  background: var(--success-bg);
  color: var(--success);
}

/* Badges */
.badge {
  display: inline-flex;
  align-items: center;
  padding: var(--space-1) var(--space-2);
  border-radius: 999px;
  background: var(--bg);
  border: 1px solid var(--border);
  color: var(--text-muted);
  font-size: var(--text-xs);
}

/* Page heading */
.page-heading {
  font-size: var(--text-xl);
  font-weight: var(--font-weight-bold);
  color: var(--text);
  margin: 0 0 var(--space-5);
}

.page-heading-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: var(--space-5);
}

.page-heading-row .page-heading {
  margin-bottom: 0;
}

.section {
  margin-top: var(--space-5);
}

.muted-text {
  color: var(--text-muted);
  font-size: var(--text-sm);
}

/* Forms */
.form-row {
  display: flex;
  gap: var(--space-4);
}

@media (max-width: 600px) {
  .form-row {
    flex-direction: column;
  }
}

.form-actions {
  display: flex;
  justify-content: flex-end;
  gap: var(--space-3);
  margin-top: var(--space-4);
}

/* Flashcard grid */
.flashcard-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: var(--space-4);
}

.flashcard-tile {
  background: var(--surface-raised);
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  padding: var(--space-4);
  display: flex;
  flex-direction: column;
  gap: var(--space-2);
}

.flashcard-tile-front {
  font-size: var(--text-base);
  color: var(--text);
  font-weight: var(--font-weight-medium);
}

.flashcard-tile-back {
  font-size: var(--text-sm);
  color: var(--text-muted);
}

.flashcard-tile-footer {
  margin-top: var(--space-2);
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.flashcard-tile-delete {
  opacity: 0;
  transition: opacity var(--transition-fast);
}

.flashcard-tile:hover .flashcard-tile-delete,
.flashcard-tile-delete:focus-visible {
  opacity: 1;
}

@media (max-width: 768px) {
  .flashcard-tile-delete {
    opacity: 1;
  }
}

/* Review */
.review-progress {
  max-width: 560px;
  margin: 0 auto var(--space-3);
  color: var(--text-faint);
  font-size: var(--text-sm);
}

.review-card {
  max-width: 560px;
  margin: 0 auto;
}

.review-front {
  font-size: var(--text-2xl);
  color: var(--text);
  margin-bottom: var(--space-5);
}

.review-divider {
  border: none;
  border-top: 1px solid var(--border);
  margin: var(--space-5) 0;
}

.review-back {
  font-size: var(--text-xl);
  color: var(--text);
  margin-bottom: var(--space-5);
}

.review-ratings {
  display: flex;
  gap: var(--space-3);
}

.review-ratings .btn {
  flex: 1;
}

.review-empty {
  text-align: center;
  color: var(--text-muted);
  padding: var(--space-8) 0;
}

/* Stats */
.stat-tile {
  max-width: 320px;
}

.stat-tile-value {
  font-size: var(--text-2xl);
  font-weight: var(--font-weight-bold);
  color: var(--text);
}

.stat-tile-label {
  font-size: var(--text-sm);
  color: var(--text-muted);
}
```

- [ ] **Step 4: Update `index.html`** — replace the inline `<style>` block with links to the three new stylesheets:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Flash Review</title>
    <link rel="stylesheet" href="./public/css/tokens.css">
    <link rel="stylesheet" href="./public/css/layout.css">
    <link rel="stylesheet" href="./public/css/components.css">
</head>
<body>
    <script src="./index.js"></script>
</body>
</html>
```

- [ ] **Step 5: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Then open `http://localhost:3000` in a browser. Expected: the page background is near-black (`#0b0e14`), text is light — the existing (not-yet-restyled) components still render with their old inline styles on top, but the page background and font have changed. No console errors. Stop the server (Ctrl+C) once confirmed.

- [ ] **Step 6: Commit**

```bash
git add flash-review-frontend/public/css/tokens.css flash-review-frontend/public/css/layout.css flash-review-frontend/public/css/components.css flash-review-frontend/index.html
git commit -m "Add design token, layout, and component stylesheets"
```

---

### Task 2: Theme FFI module + wire into Main.purs

**Files:**
- Create: `flash-review-frontend/src/Theme.purs`
- Create: `flash-review-frontend/src/Theme.js`
- Modify: `flash-review-frontend/src/Main.purs`

**Interfaces:**
- Consumes: nothing new (uses `window.localStorage` and `document.documentElement` directly).
- Produces: `Theme.initTheme :: Effect String` (reads persisted theme, applies `data-theme` to `<html>`, returns `"dark"` or `"light"`), `Theme.setTheme :: String -> Effect Unit` (applies `data-theme` and persists). Consumed by `Main.purs` (this task) and `Components/App.purs` (Task 3).

- [ ] **Step 1: Create `src/Theme.js`**

```js
export const initThemeImpl = () => {
  const saved = window.localStorage.getItem("flashreview-theme");
  const theme = saved === "light" ? "light" : "dark";
  document.documentElement.setAttribute("data-theme", theme);
  return theme;
};

export const setThemeImpl = (theme) => () => {
  document.documentElement.setAttribute("data-theme", theme);
  window.localStorage.setItem("flashreview-theme", theme);
};
```

- [ ] **Step 2: Create `src/Theme.purs`**

```purescript
module Theme
  ( initTheme
  , setTheme
  ) where

import Prelude

import Effect (Effect)

foreign import initThemeImpl :: Effect String
foreign import setThemeImpl :: String -> Effect Unit

initTheme :: Effect String
initTheme = initThemeImpl

setTheme :: String -> Effect Unit
setTheme = setThemeImpl
```

- [ ] **Step 3: Wire into `src/Main.purs`**

Full replacement:

```purescript
module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.App (component)
import Theme as Theme

main :: Effect Unit
main = HA.runHalogenAff do
  initialTheme <- liftEffect Theme.initTheme
  body <- HA.awaitBody
  runUI component initialTheme body
```

Note: `Components.App.component` is currently polymorphic in its input type (`forall input output. H.Component Query input output Aff`) and ignores its input, so passing `initialTheme :: String` here type-checks even before Task 3 changes `App.purs` to consume it.

- [ ] **Step 4: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`, open the browser devtools console, and run:

```js
document.documentElement.getAttribute('data-theme') // expect "dark"
localStorage.setItem('flashreview-theme', 'light')
```

Reload the page. Expected: `document.documentElement.getAttribute('data-theme')` is now `"light"` and the page background turns light (from the `[data-theme="light"]` override in `tokens.css`). Reload again with devtools console `localStorage.removeItem('flashreview-theme')` to reset to dark for the next task. Stop the server.

- [ ] **Step 5: Commit**

```bash
git add flash-review-frontend/src/Theme.purs flash-review-frontend/src/Theme.js flash-review-frontend/src/Main.purs
git commit -m "Add Theme FFI module for dark/light persistence"
```

---

### Task 3: App.purs sidebar shell, mobile drawer, theme toggle

**Files:**
- Modify: `flash-review-frontend/src/Components/App.purs`

**Interfaces:**
- Consumes: `Theme.setTheme :: String -> Effect Unit` (Task 2). CSS classes `app-shell`, `sidebar`, `sidebar.is-open`, `sidebar-wordmark`, `sidebar-nav`, `sidebar-footer`, `nav-link`, `nav-link.is-active`, `topbar`, `hamburger`, `drawer-backdrop`, `drawer-backdrop.is-open`, `main-content`, `auth-shell`, `auth-wordmark` (Task 1).
- Produces: `component :: forall output. H.Component Query String output Aff` (input type changed from polymorphic to `String`, the initial theme) — no other module constructs `App.component` directly, so this is safe.

- [ ] **Step 1: Replace `src/Components/App.purs`**

Full replacement:

```purescript
module Components.App where

import Prelude

import Components.FlashcardList as FlashcardList
import Components.Login as Login
import Components.Review as Review
import Components.Signup as Signup
import Components.Stats as Stats
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Theme as Theme
import Type.Proxy (Proxy(..))

data View = FlashcardsView | ReviewView | StatsView | LoginView | SignupView

derive instance eqView :: Eq View

type State = 
  { currentView :: View
  , isLoggedIn :: Boolean
  , theme :: String
  , sidebarOpen :: Boolean
  }

data Action 
  = SwitchView View
  | HandleLoginMessage Login.LoginOutput
  | HandleSignupMessage Signup.SignupOutput
  | Logout
  | ToggleSidebar
  | CloseSidebar
  | ToggleTheme

type Slots =
  ( flashcardList :: forall query. H.Slot query Unit Unit
  , review :: forall query. H.Slot query Unit Unit
  , stats :: forall query. H.Slot query Unit Unit
  , login :: H.Slot Query Login.LoginOutput Unit
  , signup :: H.Slot Query Signup.SignupOutput Unit
  )

data Query a
  = IsLoggedIn (Boolean -> a)

component :: forall output. H.Component Query String output Aff
component =
  H.mkComponent
    { initialState: \initialTheme ->
        { currentView: LoginView
        , isLoggedIn: false
        , theme: initialTheme
        , sidebarOpen: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  if state.isLoggedIn
    then renderAppShell state
    else renderAuthShell state

renderAppShell :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderAppShell state =
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "topbar") ]
        [ HH.div [ HP.class_ (HH.ClassName "sidebar-wordmark") ] [ HH.text "FlashReview" ]
        , HH.button
            [ HP.class_ (HH.ClassName "hamburger")
            , HE.onClick \_ -> ToggleSidebar
            ]
            [ HH.text "☰" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName ("drawer-backdrop" <> if state.sidebarOpen then " is-open" else ""))
        , HE.onClick \_ -> CloseSidebar
        ]
        []
    , HH.div
        [ HP.class_ (HH.ClassName "app-shell") ]
        [ renderSidebar state
        , HH.div
            [ HP.class_ (HH.ClassName "main-content") ]
            [ renderMainContent state ]
        ]
    ]

renderSidebar :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderSidebar state =
  HH.div
    [ HP.class_ (HH.ClassName ("sidebar" <> if state.sidebarOpen then " is-open" else "")) ]
    [ HH.div [ HP.class_ (HH.ClassName "sidebar-wordmark") ] [ HH.text "FlashReview" ]
    , HH.div
        [ HP.class_ (HH.ClassName "sidebar-nav") ]
        [ navLink "⚡" ReviewView "Review" state.currentView
        , navLink "🗂" FlashcardsView "Flashcards" state.currentView
        , navLink "📊" StatsView "Stats" state.currentView
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "sidebar-footer") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "nav-link")
            , HE.onClick \_ -> ToggleTheme
            ]
            [ HH.text $ if state.theme == "dark" then "☀ Light mode" else "🌙 Dark mode" ]
        , HH.a
            [ HP.class_ (HH.ClassName "nav-link")
            , HE.onClick \_ -> Logout
            ]
            [ HH.text "Logout" ]
        ]
    ]

navLink :: forall m. MonadAff m => String -> View -> String -> View -> H.ComponentHTML Action Slots m
navLink icon view label currentView =
  HH.a
    [ HP.class_ (HH.ClassName ("nav-link" <> if view == currentView then " is-active" else ""))
    , HE.onClick \_ -> SwitchView view
    ]
    [ HH.text (icon <> "  " <> label) ]

renderAuthShell :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderAuthShell state =
  HH.div
    [ HP.class_ (HH.ClassName "auth-shell") ]
    [ HH.div [ HP.class_ (HH.ClassName "auth-wordmark") ] [ HH.text "FlashReview" ]
    , case state.currentView of
        SignupView -> HH.slot _signup unit Signup.component unit HandleSignupMessage
        _ -> HH.slot _login unit Login.component unit HandleLoginMessage
    ]

renderMainContent :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderMainContent state =
  case state.currentView of
    FlashcardsView -> HH.slot_ _flashcardList unit FlashcardList.component unit
    ReviewView -> HH.slot_ _review unit Review.component unit
    StatsView -> HH.slot_ _stats unit Stats.component unit
    LoginView -> HH.slot _login unit Login.component unit HandleLoginMessage
    SignupView -> HH.slot _signup unit Signup.component unit HandleSignupMessage

_login = Proxy :: Proxy "login"
_signup = Proxy :: Proxy "signup"
_flashcardList = Proxy :: Proxy "flashcardList"
_review = Proxy :: Proxy "review"
_stats = Proxy :: Proxy "stats"

handleAction :: forall m output. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchView view -> 
    H.modify_ \st -> st { currentView = view, sidebarOpen = false }
  
  HandleLoginMessage msg -> case msg of
    Login.LoginSuccessful -> do
      H.modify_ \st -> st { isLoggedIn = true, currentView = FlashcardsView }
    
    Login.GoToSignup -> 
      H.modify_ \st -> st { currentView = SignupView }
  
  HandleSignupMessage msg -> case msg of
    Signup.SignupSuccessful -> do
      H.modify_ \st -> st { currentView = LoginView }
    
    Signup.GoToLogin ->
      H.modify_ \st -> st { currentView = LoginView }
  
  Logout ->
    H.modify_ \st -> st { isLoggedIn = false, currentView = LoginView, sidebarOpen = false }

  ToggleSidebar ->
    H.modify_ \st -> st { sidebarOpen = not st.sidebarOpen }

  CloseSidebar ->
    H.modify_ \st -> st { sidebarOpen = false }

  ToggleTheme -> do
    state <- H.get
    let newTheme = if state.theme == "dark" then "light" else "dark"
    H.liftEffect $ Theme.setTheme newTheme
    H.modify_ \st -> st { theme = newTheme }

handleQuery :: forall a m output. MonadAff m => Query a -> H.HalogenM State Action Slots output m (Maybe a)
handleQuery = case _ of
  IsLoggedIn reply -> do
    state <- H.get
    pure $ Just (reply state.isLoggedIn)
```

- [ ] **Step 2: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`. Log in (any credentials against the running backend, or just visually confirm the login card if the backend isn't running — a network error is expected and fine for this step). After a successful login: confirm a left sidebar appears with "Review" / "Flashcards" / "Stats" links, a theme-toggle row, and "Logout", and the active nav item is highlighted. Click the theme toggle — the page should switch to light immediately and stay light after a manual reload. Resize the browser window below ~768px width — the sidebar should disappear, replaced by a top bar with a "☰" button; clicking it should slide the sidebar in as a drawer with a dimmed backdrop, and clicking the backdrop or a nav link should close it. Stop the server.

- [ ] **Step 3: Commit**

```bash
git add flash-review-frontend/src/Components/App.purs
git commit -m "Rebuild App shell as a responsive sidebar with theme toggle"
```

---

### Task 4: Restyle Login and Signup

**Files:**
- Modify: `flash-review-frontend/src/Components/Login.purs`
- Modify: `flash-review-frontend/src/Components/Signup.purs`

**Interfaces:**
- Consumes: CSS classes `card`, `auth-card`, `page-heading`, `alert`, `alert-error`, `field`, `label`, `input`, `btn`, `btn-primary`, `btn-block`, `auth-footer`, `link` (Task 1). Rendered inside `App.purs`'s `.auth-shell` (Task 3) — unchanged `LoginOutput`/`SignupOutput` types, so `App.purs` needs no further changes.
- Produces: no interface changes — `LoginOutput`, `SignupOutput`, `Action`, `State` types are unchanged from before this task.

- [ ] **Step 1: Replace `src/Components/Login.purs`**

Full replacement:

```purescript
module Components.Login where

import Prelude

import API.Client as Client
import API.Types (UserCredentials(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  { username :: String
  , password :: String
  , error :: Maybe String
  , isSubmitting :: Boolean
  }

data Action 
  = SetUsername String
  | SetPassword String
  | Login
  | NavigateToSignup

type Input = Unit
type Output = LoginOutput

data LoginOutput 
  = LoginSuccessful 
  | GoToSignup

component :: forall m query. MonadAff m => H.Component query Input Output m
component = 
  H.mkComponent
    { initialState: const { username: "", password: "", error: Nothing, isSubmitting: false }
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "card auth-card") ]
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Login" ]
    , renderErrorMessage state.error
    , formField "Username" "text" state.username SetUsername
    , formField "Password" "password" state.password SetPassword
    , HH.button
        [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
        , HP.disabled state.isSubmitting
        , HE.onClick \_ -> Login
        ]
        [ HH.text $ if state.isSubmitting then "Logging in..." else "Login" ]
    , HH.div
        [ HP.class_ (HH.ClassName "auth-footer") ]
        [ HH.text "Don't have an account? "
        , HH.a
            [ HP.class_ (HH.ClassName "link")
            , HE.onClick \_ -> NavigateToSignup
            ]
            [ HH.text "Sign up" ]
        ]
    ]

renderErrorMessage :: forall action slots m. Maybe String -> H.ComponentHTML action slots m
renderErrorMessage = case _ of
  Nothing -> HH.text ""
  Just message -> 
    HH.div
      [ HP.class_ (HH.ClassName "alert alert-error") ]
      [ HH.text message ]

formField :: forall action slots m. String -> String -> String -> (String -> action) -> H.ComponentHTML action slots m
formField label type_ value onChange =
  HH.div
    [ HP.class_ (HH.ClassName "field") ]
    [ HH.label [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
    , HH.input
        [ HP.class_ (HH.ClassName "input")
        , HP.type_ (fromTypeString type_)
        , HP.value value
        , HE.onValueInput onChange
        ]
    ]

fromTypeString :: String -> HP.InputType
fromTypeString = case _ of
  "text" -> HP.InputText
  "password" -> HP.InputPassword
  _ -> HP.InputText

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetUsername username -> 
    H.modify_ \st -> st { username = username }
  
  SetPassword password -> 
    H.modify_ \st -> st { password = password }
  
  Login -> do
    state <- H.get
    if state.username == "" || state.password == "" 
      then 
        H.modify_ \st -> st { error = Just "Username and password are required" }
      else do
        H.modify_ \st -> st { isSubmitting = true, error = Nothing }
        result <- H.liftAff $ Client.login $ UserCredentials 
          { username: state.username
          , email: Nothing
          , password: state.password 
          }
        case result of
          Right _ -> do
            H.raise LoginSuccessful
          Left err -> do
            H.modify_ \st -> st { error = Just $ "Login failed: " <> err, isSubmitting = false }

  NavigateToSignup -> 
    H.raise GoToSignup
```

- [ ] **Step 2: Replace `src/Components/Signup.purs`**

Full replacement:

```purescript
module Components.Signup where

import Prelude

import API.Client as Client
import API.Types (UserCredentials(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  { username :: String
  , email :: String
  , password :: String
  , confirmPassword :: String
  , error :: Maybe String
  , isSubmitting :: Boolean
  }

data Action 
  = SetUsername String
  | SetEmail String
  | SetPassword String
  | SetConfirmPassword String
  | Signup
  | NavigateToLogin

type Input = Unit
type Output = SignupOutput

data SignupOutput 
  = SignupSuccessful 
  | GoToLogin

component :: forall m query. MonadAff m => H.Component query Input Output m
component = 
  H.mkComponent
    { initialState: const 
        { username: ""
        , email: ""
        , password: ""
        , confirmPassword: ""
        , error: Nothing
        , isSubmitting: false 
        }
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "card auth-card") ]
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Sign Up" ]
    , renderErrorMessage state.error
    , formField "Username" "text" state.username SetUsername
    , formField "Email" "email" state.email SetEmail
    , formField "Password" "password" state.password SetPassword
    , formField "Confirm Password" "password" state.confirmPassword SetConfirmPassword
    , HH.button
        [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
        , HP.disabled state.isSubmitting
        , HE.onClick \_ -> Signup
        ]
        [ HH.text $ if state.isSubmitting then "Creating account..." else "Create Account" ]
    , HH.div
        [ HP.class_ (HH.ClassName "auth-footer") ]
        [ HH.text "Already have an account? "
        , HH.a
            [ HP.class_ (HH.ClassName "link")
            , HE.onClick \_ -> NavigateToLogin
            ]
            [ HH.text "Login" ]
        ]
    ]

renderErrorMessage :: forall action slots m. Maybe String -> H.ComponentHTML action slots m
renderErrorMessage = case _ of
  Nothing -> HH.text ""
  Just message -> 
    HH.div
      [ HP.class_ (HH.ClassName "alert alert-error") ]
      [ HH.text message ]

formField :: forall action slots m. String -> String -> String -> (String -> action) -> H.ComponentHTML action slots m
formField label type_ value onChange =
  HH.div
    [ HP.class_ (HH.ClassName "field") ]
    [ HH.label [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
    , HH.input
        [ HP.class_ (HH.ClassName "input")
        , HP.type_ (fromTypeString type_)
        , HP.value value
        , HE.onValueInput onChange
        ]
    ]

fromTypeString :: String -> HP.InputType
fromTypeString = case _ of
  "text" -> HP.InputText
  "email" -> HP.InputEmail
  "password" -> HP.InputPassword
  _ -> HP.InputText

validateForm :: State -> Maybe String
validateForm state 
  | state.username == "" = Just "Username is required"
  | state.email == "" = Just "Email is required"
  | state.password == "" = Just "Password is required"
  | state.password /= state.confirmPassword = Just "Passwords don't match"
  | otherwise = Nothing

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetUsername username -> 
    H.modify_ \st -> st { username = username }
  
  SetEmail email -> 
    H.modify_ \st -> st { email = email }
  
  SetPassword password -> 
    H.modify_ \st -> st { password = password }
  
  SetConfirmPassword password -> 
    H.modify_ \st -> st { confirmPassword = password }
  
  Signup -> do
    state <- H.get
    case validateForm state of
      Just error ->
        H.modify_ \st -> st { error = Just error }
      Nothing -> do
        H.modify_ \st -> st { isSubmitting = true, error = Nothing }
        result <- H.liftAff $ Client.signup $ UserCredentials 
          { username: state.username
          , email: Just state.email
          , password: state.password 
          }
        case result of
          Right _ -> do
            H.raise SignupSuccessful
          Left err -> do
            H.modify_ \st -> st { error = Just $ "Signup failed: " <> err, isSubmitting = false }
  
  NavigateToLogin ->
    H.raise GoToLogin
```

- [ ] **Step 3: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`. Confirm the login card is centered, styled with a raised-surface background, labeled inputs, and a full-width primary button. Click "Sign up" — confirm the signup card renders the same way with four fields. Submit an empty login form — confirm the error renders as a colored alert box, not plain red text. Tab through the username/password fields — confirm a visible focus ring (indigo outline/border) on each. Stop the server.

- [ ] **Step 4: Commit**

```bash
git add flash-review-frontend/src/Components/Login.purs flash-review-frontend/src/Components/Signup.purs
git commit -m "Restyle Login and Signup with the new component classes"
```

---

### Task 5: Restyle FlashcardForm and FlashcardList

**Files:**
- Modify: `flash-review-frontend/src/Components/FlashcardForm.purs`
- Modify: `flash-review-frontend/src/Components/FlashcardList.purs`

**Interfaces:**
- Consumes: CSS classes `card`, `page-heading`, `page-heading-row`, `section`, `form-row`, `field`, `label`, `textarea`, `form-actions`, `btn`, `btn-primary`, `btn-secondary`, `btn-icon`, `alert`, `alert-error`, `alert-success`, `muted-text`, `flashcard-grid`, `flashcard-tile`, `flashcard-tile-front`, `flashcard-tile-back`, `flashcard-tile-footer`, `flashcard-tile-delete`, `badge` (Task 1).
- Produces: no interface changes — `FlashcardForm`'s `Output = Unit` and `FlashcardList`'s `Action`/`Slots` types are unchanged.

- [ ] **Step 1: Replace `src/Components/FlashcardForm.purs`**

Full replacement:

```purescript
module Components.FlashcardForm where

import Prelude

import API (Flashcard(..), createCard, wrapUUID, wrapDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { front :: String
  , back :: String
  , submitting :: Boolean
  , error :: Maybe String
  , success :: Boolean
  }

data Action
  = UpdateFront String
  | UpdateBack String
  | SubmitForm
  | ResetForm

type Output = Unit

component :: forall q i m. MonadAff m => H.Component q i Output m
component = H.mkComponent
  { initialState: \_ -> 
      { front: ""
      , back: ""
      , submitting: false
      , error: Nothing
      , success: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  HH.div
    [ HP.class_ (HH.ClassName "card") ]
    [ HH.h3 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Add New Flashcard" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-row") ]
        [ formField "Front side" state.front UpdateFront
        , formField "Back side" state.back UpdateBack
        ]
    , if state.submitting
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Submitting..." ]
        else HH.div_ []
    , case state.error of
        Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
        Nothing -> HH.div_ []
    , if state.success
        then HH.div [ HP.class_ (HH.ClassName "alert alert-success") ] [ HH.text "Card created successfully!" ]
        else HH.div_ []
    , HH.div
        [ HP.class_ (HH.ClassName "form-actions") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "btn btn-secondary")
            , HE.onClick \_ -> ResetForm
            ]
            [ HH.text "Reset" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-primary")
            , HE.onClick \_ -> SubmitForm
            , HP.disabled (state.front == "" || state.back == "" || state.submitting)
            ]
            [ HH.text "Create Card" ]
        ]
    ]
  where
    formField label value updateAction =
      HH.div
        [ HP.class_ (HH.ClassName "field") ]
        [ HH.label [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.textarea
            [ HP.class_ (HH.ClassName "textarea")
            , HP.value value
            , HE.onValueInput updateAction
            ]
        ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  UpdateFront front -> do
    H.modify_ \s -> s { front = front, success = false }

  UpdateBack back -> do
    H.modify_ \s -> s { back = back, success = false }

  ResetForm -> do
    H.modify_ \s -> s 
      { front = ""
      , back = ""
      , error = Nothing
      , success = false
      }

  SubmitForm -> do
    state <- H.get
    H.modify_ \s -> s { submitting = true, error = Nothing, success = false }
    
    now <- H.liftEffect nowDateTime
    uuid <- H.liftEffect genUUID
    
    let newCard = Flashcard
          { id: wrapUUID uuid
          , front: state.front
          , back: state.back
          , nextReview: wrapDateTime now
          , interval: 1
          , easeFactor: 2.5
          , repetitions: 0
          }
    
    result <- H.liftAff $ createCard newCard
    
    case result of
      Left err -> H.modify_ \s -> s { submitting = false, error = Just err }
      Right _ -> do
        H.modify_ \s -> s 
          { submitting = false
          , success = true
          , front = ""
          , back = ""
          }
        H.raise unit
```

- [ ] **Step 2: Replace `src/Components/FlashcardList.purs`**

Full replacement:

```purescript
module Components.FlashcardList where

import Prelude

import API (Flashcard(..), getAllCards, deleteCard)
import Components.FlashcardForm as FlashcardForm
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array (null)
import Type.Proxy (Proxy(..))

type State =
  { cards :: Array Flashcard
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | Refresh
  | DeleteCard Flashcard
  | HandleFormOutput

type Slots =
  ( flashcardForm :: forall query. H.Slot query Unit Unit )

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { cards: []
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = 
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "page-heading-row") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Flashcards" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon")
            , HE.onClick \_ -> Refresh
            ]
            [ HH.text "↻" ]
        ]
    , HH.slot (Proxy :: _ "flashcardForm") unit FlashcardForm.component unit (const HandleFormOutput)
    , HH.div
        [ HP.class_ (HH.ClassName "section") ]
        [ if state.loading
            then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
            else case state.error of
              Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
              Nothing -> renderCardList state.cards
        ]
    ]

renderCardList :: forall m. MonadAff m => Array Flashcard -> H.ComponentHTML Action Slots m
renderCardList cards =
  if null cards
    then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "No flashcards found." ]
    else HH.div [ HP.class_ (HH.ClassName "flashcard-grid") ] $ map renderCard cards

renderCard :: forall m. MonadAff m => Flashcard -> H.ComponentHTML Action Slots m
renderCard card@(Flashcard c) =
  HH.div
    [ HP.class_ (HH.ClassName "flashcard-tile") ]
    [ HH.div [ HP.class_ (HH.ClassName "flashcard-tile-front") ] [ HH.text c.front ]
    , HH.div [ HP.class_ (HH.ClassName "flashcard-tile-back") ] [ HH.text c.back ]
    , HH.div
        [ HP.class_ (HH.ClassName "flashcard-tile-footer") ]
        [ HH.span [ HP.class_ (HH.ClassName "badge") ] [ HH.text $ show c.repetitions <> " reps" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon flashcard-tile-delete")
            , HE.onClick \_ -> DeleteCard card
            ]
            [ HH.text "🗑" ]
        ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Refresh

  Refresh -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getAllCards
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right cards -> H.modify_ \s -> s { loading = false, cards = cards }

  DeleteCard (Flashcard card) -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff $ deleteCard card.id
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right _ -> handleAction Refresh
      
  HandleFormOutput -> do
    handleAction Refresh
```

- [ ] **Step 3: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`, navigate to Flashcards. Confirm the "Add New Flashcard" form renders as a card with front/back textareas side-by-side (on a wide window), and stacked when the window is narrowed below ~600px. Add a card (requires the backend running) and confirm it appears as a tile in a responsive grid, with a "reps" badge and a trash-icon delete button that's hidden until you hover the tile (on desktop) or always visible (narrow window). Delete it and confirm it disappears. Stop the server.

- [ ] **Step 4: Commit**

```bash
git add flash-review-frontend/src/Components/FlashcardForm.purs flash-review-frontend/src/Components/FlashcardList.purs
git commit -m "Restyle flashcard form and list as a responsive grid"
```

---

### Task 6: Restyle Review

**Files:**
- Modify: `flash-review-frontend/src/Components/Review.purs`

**Interfaces:**
- Consumes: CSS classes `page-heading`, `muted-text`, `alert`, `alert-error`, `review-empty`, `review-progress`, `card`, `review-card`, `review-front`, `review-divider`, `review-back`, `review-ratings`, `btn`, `btn-primary`, `btn-block`, `btn-danger-outline`, `btn-secondary`, `btn-success-outline` (Task 1).
- Produces: no interface changes — `Action`/`State` types unchanged.

- [ ] **Step 1: Replace `src/Components/Review.purs`**

Full replacement:

```purescript
module Components.Review where

import Prelude

import API (Flashcard(..), ReviewResult(..), getReviewQueue, submitReview)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array ((!!), length, null)

type State =
  { queue :: Array Flashcard
  , currentIndex :: Int
  , showAnswer :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | LoadReviewQueue
  | ShowAnswer
  | SubmitRating Int
  | NextCard

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { queue: []
      , currentIndex: 0
      , showAnswer: false
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  HH.div_
    [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Review Cards" ]
    , if state.loading
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
          Nothing -> 
            if null state.queue
              then HH.div [ HP.class_ (HH.ClassName "review-empty") ] [ HH.text "No cards to review!" ]
              else renderReview state
    ]

renderReview :: forall m. State -> H.ComponentHTML Action () m
renderReview state =
  let
    currentCard = state.queue !! state.currentIndex
  in
    case currentCard of
      Nothing -> HH.div [ HP.class_ (HH.ClassName "review-empty") ] [ HH.text "Review complete!" ]
      Just (Flashcard card) -> 
        HH.div_
          [ HH.div
              [ HP.class_ (HH.ClassName "review-progress") ]
              [ HH.text $ show (state.currentIndex + 1) <> " of " <> show (length state.queue) ]
          , HH.div
              [ HP.class_ (HH.ClassName "card review-card") ]
              [ HH.div [ HP.class_ (HH.ClassName "review-front") ] [ HH.text card.front ]
              , if state.showAnswer
                  then 
                    HH.div_
                      [ HH.hr [ HP.class_ (HH.ClassName "review-divider") ]
                      , HH.div [ HP.class_ (HH.ClassName "review-back") ] [ HH.text card.back ]
                      , HH.div
                          [ HP.class_ (HH.ClassName "review-ratings") ]
                          [ ratingButton "btn-danger-outline" 1 "Hard"
                          , ratingButton "btn-secondary" 3 "Good"
                          , ratingButton "btn-success-outline" 5 "Easy"
                          ]
                      ]
                  else 
                    HH.button
                      [ HP.class_ (HH.ClassName "btn btn-primary btn-block")
                      , HE.onClick \_ -> ShowAnswer
                      ]
                      [ HH.text "Show Answer" ]
              ]
          ]
  where
    ratingButton variantClass rating label =
      HH.button
        [ HP.class_ (HH.ClassName ("btn " <> variantClass))
        , HE.onClick \_ -> SubmitRating rating
        ]
        [ HH.text label ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadReviewQueue

  LoadReviewQueue -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getReviewQueue
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right cards -> H.modify_ \s -> s { loading = false, queue = cards, currentIndex = 0, showAnswer = false }

  ShowAnswer -> do
    H.modify_ \s -> s { showAnswer = true }

  SubmitRating rating -> do
    state <- H.get
    case state.queue !! state.currentIndex of
      Nothing -> pure unit
      Just (Flashcard card) -> do
        H.modify_ \s -> s { loading = true, error = Nothing }
        result <- H.liftAff $ submitReview card.id (ReviewResult { rating })
        case result of
          Left err -> H.modify_ \s -> s { loading = false, error = Just err }
          Right _ -> handleAction NextCard

  NextCard -> do
    state <- H.get
    let nextIndex = state.currentIndex + 1
    if nextIndex >= length state.queue
      then H.modify_ \s -> s { loading = false, queue = [], currentIndex = 0, showAnswer = false }
      else H.modify_ \s -> s { loading = false, currentIndex = nextIndex, showAnswer = false }
```

- [ ] **Step 2: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`, navigate to Review (requires the backend running with at least one due card). Confirm: a "N of M" progress line above a centered card, a large front-text, a full-width "Show Answer" button. Click it — confirm a divider, the back text, and three rating buttons that are visually distinct (Hard has a red-ish outline, Good is neutral, Easy has a green-ish outline). Submit a rating and confirm it advances to the next card or shows "Review complete!" when the queue is exhausted. With an empty queue (or backend down and an error), confirm the "No cards to review!" / error states render centered and readable. Stop the server.

- [ ] **Step 3: Commit**

```bash
git add flash-review-frontend/src/Components/Review.purs
git commit -m "Restyle Review with progress indicator and difficulty-colored ratings"
```

---

### Task 7: Restyle Stats

**Files:**
- Modify: `flash-review-frontend/src/Components/Stats.purs`

**Interfaces:**
- Consumes: CSS classes `page-heading-row`, `page-heading`, `btn`, `btn-icon`, `muted-text`, `alert`, `alert-error`, `card`, `stat-tile`, `stat-tile-value`, `stat-tile-label` (Task 1).
- Produces: no interface changes — `Action`/`State` types unchanged (the standalone bottom "Refresh" button is replaced by a header icon button; the `RefreshStats` action and its behavior are unchanged).

- [ ] **Step 1: Replace `src/Components/Stats.purs`**

Full replacement:

```purescript
module Components.Stats where

import Prelude

import API (Stats(..), getStats)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { stats :: Maybe Stats
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | RefreshStats

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { stats: Nothing
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  HH.div_
    [ HH.div
        [ HP.class_ (HH.ClassName "page-heading-row") ]
        [ HH.h2 [ HP.class_ (HH.ClassName "page-heading") ] [ HH.text "Statistics" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-icon")
            , HE.onClick \_ -> RefreshStats
            ]
            [ HH.text "↻" ]
        ]
    , if state.loading
        then HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "Loading..." ]
        else case state.error of
          Just err -> HH.div [ HP.class_ (HH.ClassName "alert alert-error") ] [ HH.text $ "Error: " <> err ]
          Nothing -> renderStats state.stats
    ]

renderStats :: forall m. Maybe Stats -> H.ComponentHTML Action () m
renderStats Nothing = HH.div [ HP.class_ (HH.ClassName "muted-text") ] [ HH.text "No stats available." ]
renderStats (Just (Stats s)) =
  HH.div
    [ HP.class_ (HH.ClassName "card stat-tile") ]
    [ HH.div [ HP.class_ (HH.ClassName "stat-tile-value") ] [ HH.text $ show s.dueToday ]
    , HH.div [ HP.class_ (HH.ClassName "stat-tile-label") ] [ HH.text "Cards due today" ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction RefreshStats

  RefreshStats -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff getStats
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right stats -> H.modify_ \s -> s { loading = false, stats = Just stats }
```

- [ ] **Step 2: Build and verify**

Run: `cd flash-review-frontend && npm run build && npm run bundle && npm run serve`

Open `http://localhost:3000`, navigate to Stats. Confirm a stat tile card with a large number and a "Cards due today" label below it, and a refresh icon button next to the "Statistics" heading that reloads the count on click. Stop the server.

- [ ] **Step 3: Commit**

```bash
git add flash-review-frontend/src/Components/Stats.purs
git commit -m "Restyle Stats as a stat tile"
```

---

### Task 8: Full walkthrough verification

**Files:** none (verification only).

**Interfaces:** none — this task exercises the app end to end using every interface produced by Tasks 1-7.

- [ ] **Step 1: Start the full stack**

Follow the backend's existing run instructions (see `flash-review-backend/README.md` or repo root docs) to get the API running, then:

```bash
cd flash-review-frontend && npm run build && npm run bundle && npm run serve
```

- [ ] **Step 2: Walk every view in dark mode (default)**

Open `http://localhost:3000` in a fresh (unauthenticated) browser profile/incognito window. Confirm, in order: Login card renders correctly → "Sign up" navigates to Signup and back via "Login" link → create an account → log in → sidebar appears with Review/Flashcards/Stats → Flashcards: add a card, confirm it appears in the grid, delete it → Review: confirm the queue renders, show answer, submit a rating, reach the empty/complete state → Stats: confirm the due-count tile and refresh button work → Logout returns to the Login card.

- [ ] **Step 3: Toggle to light mode and repeat spot checks**

Click the theme toggle in the sidebar footer. Confirm the whole app switches to the light palette (white/near-white surfaces, dark text, indigo accent) with no unreadable text or invisible borders. Reload the page — confirm it stays in light mode. Spot-check the Flashcards grid and Review card in light mode for contrast.

- [ ] **Step 4: Resize to mobile width**

With devtools open, set the viewport to ~375px wide. Confirm: the sidebar is replaced by a top bar with a hamburger button; opening it slides in the sidebar over the content with a dimmed backdrop; the flashcard form's front/back fields stack vertically; flashcard tiles' delete buttons are visible without hovering (no hover on touch).

- [ ] **Step 5: Keyboard focus check**

Using only the Tab key, tab through the Login form fields and submit button — confirm a visible focus ring on each. Log in, go to Review, show an answer, and tab through the three rating buttons — confirm each shows a visible focus ring.

- [ ] **Step 6: Final commit (if any fixes were needed)**

If Steps 2-5 surfaced any issues, fix them in the relevant component/CSS file, re-run Steps 2-5 for the affected area, then:

```bash
git add -A
git commit -m "Fix issues found in full redesign walkthrough"
```

If no issues were found, this task requires no commit — the branch is ready for review.

---

## Self-Review Notes

- **Spec coverage:** every spec section (tokens, layout/nav, per-page redesign, cross-cutting states, testing) maps to a task above. Icons were simplified from "SVG" to "Unicode glyph" — documented as a deliberate deviation in Global Constraints, not a silent gap.
- **Placeholder scan:** no TBD/TODO markers; every step contains complete file content or exact commands.
- **Type consistency:** `Theme.initTheme :: Effect String` / `Theme.setTheme :: String -> Effect Unit` (Task 2) match their usage in `Main.purs` (Task 2) and `App.purs` (Task 3) exactly. `App.purs`'s `component :: forall output. H.Component Query String output Aff` (Task 3) matches the `runUI component initialTheme body` call introduced in Task 2. CSS class names are used identically across Task 1 (definition) and Tasks 3-7 (consumption) — cross-checked `sidebar`/`is-open`, `nav-link`/`is-active`, `drawer-backdrop`/`is-open`, `card`/`auth-card`/`review-card`/`stat-tile`, `btn`/`btn-primary`/`btn-secondary`/`btn-danger-outline`/`btn-success-outline`/`btn-icon`/`btn-block`, `alert`/`alert-error`/`alert-success`, `field`/`label`/`input`/`textarea`, `flashcard-grid`/`flashcard-tile`/`flashcard-tile-front`/`flashcard-tile-back`/`flashcard-tile-footer`/`flashcard-tile-delete`, `review-progress`/`review-front`/`review-divider`/`review-back`/`review-ratings`/`review-empty`, `page-heading`/`page-heading-row`/`section`/`muted-text`/`form-row`/`form-actions`/`badge`/`auth-footer`/`link`/`hamburger`/`topbar`/`sidebar-wordmark`/`sidebar-nav`/`sidebar-footer`/`auth-shell`/`auth-wordmark`/`app-shell`/`main-content` — all defined once in Task 1 and referenced verbatim thereafter.
