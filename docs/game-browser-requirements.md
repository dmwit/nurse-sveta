# Game Browser Requirements

This document captures agreed requirements for fleshing out `bin/game-browser.hs`.

## Scope By Version

### v1

- Manual creation of Dr. Mario game trees only.
- Start from a blank tree.
- Add moves and branches.
- Use the existing action set in `bin/game-browser.hs`:
  - `GenerateLevel`
  - `Lock`
- Illegal moves must be impossible in the UI (prevent selection/input of illegal actions).
- UI priorities:
  - board view
  - variation tree view
  - tools pane
- Clicking a node in the variation tree must jump to that board state.
- Navigation can be rudimentary beyond tree-click jump.
- Tree UI should prioritize graphical move representation.
  - Use text only if needed for disambiguation.
- Branch insertion behavior:
  - If a new move does not match any existing continuation at the current position, append a new variation after all existing variations at that point.
- Node deletion is allowed with destructive confirmation.
  - No undo; confirmation warns the user deletion is permanent.

### v1.5

- Support both abstraction levels for actions:
  - High-level/intent actions (example: lock a pill at a target location with cleanup; reset to specific seed/level).
  - Low-level actions (example: move pill left one column; animate trash falling one row).
- Start with a very small move/action set and expand later.

### v2

- Add machine-generated analysis overlays geared toward Ms. Mendel workflows:
  - show board locations where specific patterns match
  - show local high/low evaluation regions (heatmap-like)
- Add full pill-sequence preview where sensible/possible.

### v3

- Add manual annotations:
  - highlight specific board locations
  - mark moves as good/bad
  - mark positions as good/bad
- Add save/load support.
- Add position metadata and next-pill lookahead support.

### v4 (or later / maybe never)

- Add undo/redo.
- Add advanced navigation:
  - stepping controls
  - jump-to-move-number
  - auto-play
  - jump to priority events

## Data/Compatibility Constraints

- No existing format compatibility is required.
- Saving/loading format can be designed from scratch.

## Scale/Performance Expectations

- One game open at a time.
- Typical tree size target:
  - ~2000 moves on main line
  - ~100 short variations
- No special lazy-loading/caching infrastructure is required for this target.

## Architecture Guidance

- Choose integration points based on desired behavior, not on forcing reuse of specific current modules.
