# Design

The decisions behind waybar-pokemon, and the questions that are still open. Each decision says why it was made and when to revisit it.

## Goal

Small Pokémon live in a strip of the desktop, usually in the status bar. They drop in, stand around, jiggle like the party icons in the games and walk around. The user spawns specific or random ones and despawns them again. As in the games, one in 4096 is shiny.

It should be charming, not distracting, and cheap enough to run all day.

## Language: Python, Rust later if needed

**Decision:** Python.

- Pillow does the image work (compositing, PNG encoding) in C. Python only moves a few numbers per frame.
- Quick to change while the design is still moving, and easy for others to read and contribute to.
- Packaging is simple: uv for development, a few lines of Nix.

**Cost:** measured with six Pokémon at 10 frames per second, about 1 % of one CPU core and 29 MB of memory. Most of the memory is the interpreter and Pillow.

**Revisit** when the resource use matters to users or the runtime dependency gets in the way of distributing it. Then the candidate is **Rust**: a single binary without runtime, good libraries for images, HTTP, JSON and the command line, and easy packaging. C was ruled out (image handling, downloads, JSON and memory management by hand), Zig too (not at 1.0 yet, breaking changes between releases).

Before rewriting anything, use the cheaper wins: stop rendering while nothing moves (e.g. an empty party), and lower the frame rate if it still looks good.

## Architecture: engine and content

**Decision:** the code is split into an engine that knows nothing about Pokémon and the Pokémon content on top of it. Nothing in `engine/` imports from `pokemon/`.

`engine/` runs a classic game loop, a fixed number of times per second (`loop.py`):

1. **Input:** `Scene.sync()` adds and removes entities, e.g. after a command.
2. **Update:** every entity advances by the elapsed time, `update(dt, stage)`. This is the only step that changes state, including random choices and animation clocks.
3. **Draw:** every entity paints itself onto a fresh transparent frame, `draw(frame, stage)`, in the order it was added. Drawing only reads state.
4. **Present:** an output shows the frame (`output.py`).

The engine also provides the stage (size and random generator), sprites facing left and right, and the `Walker`: an entity that drops in, stands around (with the jiggle) and walks along the ground.

`pokemon/` is the content:

- `pokedex.py`: species, generations, sprite download, shiny chance
- `party.py`: the party file
- `habitat.py`: the scene; it follows the party file
- `creature.py`: a Pokémon on the stage, a walker
- `sparkle.py`: the sign of a shiny one, drawn on top of its sprite

**Why:** keeping update and draw apart keeps drawing simple and makes new kinds of entities, effects or small games possible without touching the loop or the outputs. The abstraction costs one function call per entity per frame.

**Scope:** only what the Pokémon use today. The interfaces grow when a second kind of content actually exists, not before.

## Outputs and integrations

**Decision:** the engine produces frames; outputs present them. The only output today is a PNG file, rewritten only when the picture changed, replaced atomically, and kept in `/dev/shm` (memory). Anything that can show an image file and reload it regularly can show the habitat: waybar's image module today; eww, Quickshell, AGS or Conky are possible. Text-only bars (polybar, i3bar/swaybar, yambar) cannot. The README lists the integrations.

A file has a cost on the reader's side: waybar reads and decodes it every interval, even when it did not change. Outputs that avoid this are the reason for the open question below.

Further outputs to consider:

- XPM files, for xmobar
- the terminal, with kitty's graphics protocol
- an overlay window of its own (below)

## Open question: a Wayland overlay

An output that is its own small window on a [layer-shell](https://wayland.app/protocols/wlr-layer-shell-unstable-v1) surface, drawn directly instead of through a file. It would work without any bar and would be the most efficient output.

**Positioning is undecided.** A free-floating window is more distracting than charming, so the overlay should be anchored like a bar:

- a strip along a screen edge (top or bottom, full width or a part of it), with a margin, e.g. directly below the status bar, so the Pokémon walk along its edge;
- no exclusive zone, so windows are not pushed aside;
- clicks pass through (empty input region), so it never covers anything clickable; spawning and despawning stay commands;
- the background or bottom layer (only visible on an empty desktop, like desktop pets) or the top layer (always visible, but above windows).

To decide with a prototype: which edge, which layer, and whether it should react to the pointer at all.

## Sprites: downloaded, never bundled

**Decision:** names and sprites are downloaded at runtime from GitHub and cached; the project contains none of them. The sprites are Nintendo's artwork, so they are not distributed with the code.

- Icons from [PokéSprite](https://github.com/msikma/pokesprite), small enough for a bar: the classic generation 7 icons first, then generation 8, then [PokeAPI's](https://github.com/PokeAPI/sprites) larger front sprites for the newest Pokémon.
- The cache directory is named after the icon sets, so changing them downloads fresh sprites.
- `cache_all_sprites` downloads everything in the background (about 2000 small files), so spawning works offline afterwards.
- Sprites are only ever shrunk to fit the habitat, never enlarged, so sizes keep their proportions.

## Commands and state

**Decision:** the party is a JSON file. Commands change it under a file lock; the daemon checks its modification time every frame and syncs. The party survives restarts.

**Why:** no sockets or protocol, commands work while the daemon is not running, and the state is easy to inspect.

**Revisit** if commands need answers from the daemon or more interaction than spawning and despawning.

## Name

**Decision:** `waybar-pokemon` for now. It says exactly what it is, and "pokemon" helps people find it.

**Revisit** when a second output (e.g. the overlay) ships and waybar is no longer the main way to use it.

## Packaging

**Decision:** uv with its build backend (`uv_build`), and a Nix package built from the same `pyproject.toml`. The project is still developed inside a dotfiles repository; it will become its own repository with the Nix package.
