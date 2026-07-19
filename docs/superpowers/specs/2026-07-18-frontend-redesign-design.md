# FlashReview Frontend Redesign — Design Spec

Date: 2026-07-18

## Goal

Redesign the `flash-review-frontend` UI (PureScript/Halogen) to be slicker, cleaner, and more modern, while keeping all existing functionality and business logic unchanged. This is a pure presentation-layer redesign — no API, routing, or state-management changes beyond what's needed to drive theming and a collapsible sidebar.

## Current State

- PureScript/Halogen app (`spago`/`esbuild`), served by a plain Express server (`server.js`).
- 6 components: `App` (shell/router), `Login`, `Signup`, `FlashcardList`, `FlashcardForm`, `Review`, `Stats`.
- All styling is inline, written per-element via the `halogen-css` DSL (e.g. `CSS.padding (CSS.px 20.0) ...`). No stylesheet files exist. `index.html` has one inline `<style>` block (margin reset + Arial).
- Visual style today: flat blue (#2196F3) top header with text nav links, bare bordered boxes for cards/lists, no hover/focus states, no dark mode, no responsive behavior, no icons.

## Decisions

| Area | Decision |
|---|---|
| Styling implementation | Real CSS files, split by concern (not the halogen-css DSL, not a single monolithic stylesheet) |
| Aesthetic direction | Clean, minimal SaaS |
| Theme | Dark by default, with a light-mode toggle (persisted) |
| Accent color | Indigo |
| Navigation layout | Left sidebar (collapses to a top bar + drawer on mobile) |
| Redesign scope | All 6 components (Login, Signup, FlashcardList, FlashcardForm, Review, Stats) |
| Typography | System font stack — no webfont, no external network dependency |
| Responsiveness | Required — usable down to ~375px mobile width |

## Architecture

New files under `flash-review-frontend/`:

- `public/css/tokens.css` — CSS custom properties: color scale (dark on `:root`, light override under `[data-theme="light"]`), spacing scale, radii, shadows, font sizes/weights, transition durations.
- `public/css/layout.css` — the app shell: sidebar, mobile top-bar/drawer, content area, responsive breakpoints (~768px).
- `public/css/components.css` — buttons, inputs/form fields, cards, nav links, flashcard tiles, rating buttons, alert/error boxes, loading states.

`index.html` links all three stylesheets (tokens first) and keeps a minimal inline reset (box-sizing/margin). No `server.js` change is needed: it already does `app.use(express.static(__dirname))`, so `public/css/*.css` is served automatically at `/public/css/*.css`.

A new `src/Theme.purs` FFI module (PureScript + a small `.js` foreign file) provides:
- `initTheme :: Effect Unit` — reads the saved theme from `localStorage` (falls back to `"dark"`), sets `data-theme` on `<html>`.
- `setTheme :: String -> Effect Unit` — sets `data-theme` on `<html>` and persists to `localStorage`.

This is the only new PureScript/JS glue required. Every other component change is replacing `HCSS.style do ...` blocks with `HP.class_ (HH.ClassName "...")` references into the new stylesheets, plus the markup restructuring described below.

## Design Tokens

**Color system (dark, default):**
- Surfaces: `--bg` #0b0e14 (page), `--surface` #11151d (sidebar/header), `--surface-raised` #151a24 (cards/panels), `--border` #1f2530
- Text: `--text` #e6e9ef (primary), `--text-muted` #8b93a5 (secondary), `--text-faint` #5b6272 (disabled/tertiary)
- Accent (indigo): `--accent` #7c9eff, `--accent-hover` #95afff, `--accent-muted` #2a3142 (e.g. active-nav background)
- Semantic: `--danger` #f4534a / `--danger-bg` #3a1f1f, `--success` #4ade80

**Color system (light, `[data-theme="light"]` override):**
- `--bg` #f7f8fa, `--surface` #ffffff, `--surface-raised` #ffffff, `--border` #e4e7ec
- `--text` #1a1d24, `--text-muted` #5b6272
- `--accent` #5b7cfa (deepened slightly for AA contrast on white)
- Danger/success adjusted for AA contrast on a white background.

**Spacing scale:** `--space-1` (4px) through `--space-8` (48px), replacing today's ad hoc pixel values.

**Type scale:** system font stack (`-apple-system, "Segoe UI", Roboto, Inter, sans-serif`); sizes `--text-xs` (12px) through `--text-2xl` (28px); `--font-weight-medium` / `--font-weight-bold`.

**Radii/shadows:** `--radius-sm` (6px, inputs/buttons), `--radius-md` (10px, cards), `--radius-lg` (14px); single `--shadow-card` (used mainly in light theme — dark theme relies on borders rather than shadows, which read poorly on dark backgrounds).

## Layout & Navigation

`Components/App.purs` shell becomes a flex row for the logged-in state: a fixed-width (~220px) sidebar (wordmark, nav links — Review / Flashcards / Stats — spacer, theme toggle + Logout pinned to the bottom) and a scrollable main content area.

- Active nav item: `--accent-muted` background + `--accent` text/icon (replacing today's bold+underline).
- Each nav link gets a small inline SVG icon (no icon font/library — keeps zero new external dependencies).
- **Responsive (≤768px):** sidebar collapses to a slim top bar (wordmark + hamburger). Tapping opens a fixed-position drawer with an overlay backdrop; closes on backdrop tap or nav selection. Driven by a `sidebarOpen :: Boolean` field in `App.purs` state — no new library.
- **Logged-out state** (Login/Signup): no sidebar — a centered card on `--bg`, with a small wordmark above it instead of the persistent header.
- Theme toggle: sun/moon icon button in the sidebar footer; calls `Theme.setTheme` and flips local Halogen state so the icon reflects current theme.

## Page-by-Page Redesign

**Review** (core flow): large centered card (max-width ~560px), prominent front text (`--text-2xl`), divider before the revealed answer. "Show Answer" is a full-width primary button. Rating buttons are visually distinct by difficulty: Hard uses a `--danger`-tinted outline, Good uses neutral/accent, Easy uses a `--success`-tinted style — replacing today's identical blue buttons. A thin "N of M" progress indicator sits above the card. Empty/complete states get a centered message instead of a bare text line.

**Flashcards** (list + form): add-card form is a compact card at the top (front/back inputs side-by-side on wide screens, stacked on mobile) with a primary "Add" button. The list becomes a responsive grid of tiles (not stacked full-width blocks); each tile shows front/back, a muted "repetitions" badge, and a ghost/icon delete button (hover-revealed on desktop, always visible on touch) styled `--danger` on hover rather than a solid red button at rest.

**Stats**: presented as a stat tile — large number, muted label, subtle accent icon — matching the visual language of flashcard/review cards. "Refresh" becomes a small icon-button.

**Login / Signup**: centered card, labeled inputs on `--surface-raised` with `--accent` `:focus-visible` rings, full-width primary submit button, error messages as an inline alert (icon + `--danger-bg` background) instead of plain red text.

**Cross-cutting:** every interactive element gets real `:hover` / `:focus-visible` / `:disabled` states and a ~120ms transition on color/background/transform (e.g. buttons lift 1px on hover).

## Error Handling

No changes to error handling logic — existing `Either String a` error states from the API layer continue to surface the same messages. Only their presentation changes (styled inline alert instead of plain red text), per the Login/Signup and cross-cutting sections above.

## Testing & Verification

This is a presentation-layer-only change: no PureScript business logic, API calls, or state-management logic changes beyond the new `sidebarOpen` boolean and `Theme` FFI module. Existing behavior (auth flow, CRUD, review submission, stats fetch) is unchanged.

Verification is manual/visual, run via the existing `npm run build && npm run bundle && npm run serve`:
- Walk every view (Login → Signup → back, Flashcards add/delete, Review through a full queue including empty/complete states, Stats refresh) in both themes.
- Check both a desktop width and a ~375px mobile width (sidebar collapse/drawer behavior).
- Confirm keyboard focus states are visible (tab through the login form and review rating buttons).

No new automated tests are planned — this matches the existing test setup (`Test.Main` covers logic, not rendering), and there is no rendering/snapshot test infrastructure in this codebase to extend.

## Out of Scope

- Any change to API contracts, routing logic, or Halogen component `Action`/`Query` types beyond what's needed for the sidebar-collapse toggle and theme persistence.
- Automated visual regression testing.
- Backend (`flash-review-backend`) changes.
- A logo/icon asset beyond simple inline SVGs — no image assets or icon library are introduced.
