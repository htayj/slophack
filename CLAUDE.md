# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Common Lisp roguelike game "flaghack". Reference implementation (incomplete TypeScript/Effect.ts version) at `~/projects/flaghack-ink/flag-hack`.

## Technology Stack

- **Language**: Common Lisp (SBCL 2.5.8)
- **UI**: croatoan (ncurses bindings via Guix)
- **Build**: ASDF system definition (`flaghack.asd`)
- **Package management**: Guix (croatoan is in the system profile, no Quicklisp)
- **Architecture**: Single-process, no client/server split — direct ncurses rendering

## Common Commands

```bash
# Load and run the game
sbcl --eval '(require :asdf)' \
     --eval '(push #p"/home/tay/projects/cl/flaghack/" asdf:*central-registry*)' \
     --eval '(asdf:load-system :flaghack)' \
     --eval '(flaghack:run)'

# Compile-check only (non-interactive)
sbcl --non-interactive --eval '(require :asdf)' \
     --eval '(push #p"/home/tay/projects/cl/flaghack/" asdf:*central-registry*)' \
     --eval '(asdf:load-system :flaghack)'
```

## Code Architecture

All code is in the `:flaghack` package. Files load serially via ASDF:

- `package.lisp` — Package definition
- `position.lisp` — `pos` struct (x,y,z), direction vectors (`+up+`, `+down+`, etc.), `pos-shift`, `collide-p`, `direction-to-delta`
- `entities.lisp` — Entity structs (`entity` base, `creature` with name, `terrain` with variant), constructors for all types (player, hippie, acid-kop, flag, wall, tent-wall, floor-tile, etc.), type predicates (`creature-p`, `item-p`, `impassable-p`)
- `gamestate.lisp` — `game-state` struct (hash-table world + log), queries (`world-get`, `get-player`, `get-entities-at`, `get-items-at`, `get-inventory`, `get-creatures`)
- `world.lisp` — World generation: `campground-level` (perimeter walls, floors, 4 tents, fire ring, items, hippie), `bsp-gen-level` (BSP dungeon), wall variant determination, `find-random-floor`
- `actions.lisp` — `move-entity` (with collision), `pickup-item`, `drop-item`, `execute-action` dispatcher
- `ai.lisp` — `hippie-ai`/`acid-kop-ai` (rectangular patrol), `plan-all-ai`
- `display.lisp` — `entity-tile` returns (char fgcolor attrs), `render-world` draws entities via priority grid, `render-status` shows inventory/items/log
- `main.lisp` — `run` entry point, `game-loop` (render→input→AI→execute), vi-style keybindings (hjklyubn, g=pickup, d=drop, q=quit)

## Croatoan API Notes

- Colors passed directly: `:fgcolor :white :bgcolor :black` (no numbered color pairs)
- Bold via `:attributes '(:bold)`
- `(croatoan:add-char scr ch :y y :x x :fgcolor fg :bgcolor :black)`
- `(croatoan:with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil) ...)`

## Entity System

All entities stored in a hash-table keyed by string. Every entity has: `key`, `at` (pos), `container` (string: "world" or creature key), `tag` (keyword). Items in inventory have `container` set to the creature's key.
