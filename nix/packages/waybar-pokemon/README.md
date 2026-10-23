# waybar-pokemon

Little Pokémon walking around in a [waybar](https://github.com/Alexays/Waybar) module, inspired by the vscode-pokemon extension. Spawn specific or random Pokémon, despawn them again, and with the same chance as in the games (1 in 4096), one turns out shiny and sparkles.

Prototype: it is developed inside a dotfiles repository and meant to become its own repository and Nix package later.

## How it works

- `waybar-pokemon daemon` simulates the Pokémon (dropping in, standing, walking, turning at the edges) and renders the habitat as a PNG strip several times per second. It only rewrites the file when the picture changed.
- waybar's **image module** shows that file and reads it again in a short interval. Its `size` is the habitat's width (waybar scales the image into a `size` × `size` box, keeping its aspect ratio).
- The commands `spawn` and `despawn` change the party, a JSON file the daemon watches. The party is kept across restarts.
- Names come from the [PokeAPI](https://github.com/PokeAPI) species table. Sprites are the games' small box icons (normal and shiny) from [PokéSprite](https://github.com/msikma/pokesprite): the classic icons of generation 7 where there is one, else those of generation 8; for the newest Pokémon, which have no icon there, PokeAPI's front sprites are used. Everything is downloaded from GitHub on first use and cached. Sprites are never enlarged and only shrunk to fit the height of the habitat.
- Standing Pokémon jiggle like the party icons in the games; walking ones hop with every step.

Code (`src/waybar_pokemon/`), see [DESIGN.md](DESIGN.md) for the reasoning:

| Module | Purpose |
|---|---|
| `engine/` | independent of the Pokémon: the main loop (input, update, draw, present), stage, scene and entities, sprites, the walker (dropping in, standing, walking), and outputs (the PNG file) |
| `pokemon/` | the Pokémon: `pokedex.py` (species, generations, sprite download, shiny chance), `party.py` (the party file), `habitat.py` (the scene that follows the party), `creature.py` (a Pokémon on the stage), `sparkle.py` (the sign of a shiny one) |
| `daemon.py` | puts the habitat into the engine's loop with the PNG output |
| `cli.py` | command line |
| `config.py`, `paths.py` | settings and file locations |

## Usage

```
waybar-pokemon daemon               # run in the background, e.g. from the compositor's autostart
waybar-pokemon spawn [NAME|NUMBER]  # without an argument: a random Pokémon
waybar-pokemon spawn pikachu --shiny
waybar-pokemon despawn [NAME|NUMBER]  # without an argument: the newest one; --all for everyone
waybar-pokemon spawn --generations 1-4  # a random one from generations 1 to 4
waybar-pokemon list
waybar-pokemon download             # download all sprites now (about 2000 small files)
waybar-pokemon spawn-menu           # choose with rofi (--menu for another dmenu-like command)
waybar-pokemon despawn-menu
```

`--notify` shows the result as a desktop notification ("A wild Pikachu appeared!"), which is useful when the commands are run from waybar.

## Configuration

`~/.config/waybar-pokemon/config.json`, read by the daemon and the commands. All fields are optional:

```json
{
  "width": 240,
  "height": 30,
  "fps": 10,
  "frame": "/dev/shm/waybar-pokemon-USER/frame.png",
  "cache_all_sprites": false,
  "random_generations": []
}
```

| Field | Meaning |
|---|---|
| `width`, `height` | size of the habitat in pixels |
| `fps` | frames per second |
| `frame` | where the frame is written; the default is in `/dev/shm` |
| `cache_all_sprites` | `true`: the daemon downloads the sprites of all Pokémon (normal and shiny) in the background, so any Pokémon can be spawned without internet. `false`: a sprite is downloaded when its Pokémon is spawned for the first time. `waybar-pokemon download` does the same once, in the foreground. |
| `random_generations` | generations that random spawns pick from, e.g. `[1, 2, 3, 4]`; empty means all. `--generations` overrides it for one command. Spawning a Pokémon by name is not restricted. |

The waybar module must match it:

```json
"image#pokemon": {
  "path": "/dev/shm/waybar-pokemon-USER/frame.png",
  "size": 240,
  "interval": 0.1,
  "tooltip": false,
  "on-click": "waybar-pokemon spawn-menu --notify",
  "on-click-middle": "waybar-pokemon spawn --notify",
  "on-click-right": "waybar-pokemon despawn-menu --notify"
}
```

## Development

The project uses [uv](https://docs.astral.sh/uv/): `uv run waybar-pokemon ...` runs it from the source tree, `uv build` builds it. The Nix package (`default.nix`) builds it with uv's build backend and the nixpkgs version of Pillow.

## Files

- `~/.cache/waybar-pokemon/`: species table and sprites
- `~/.local/state/waybar-pokemon/party.json`: the party
- `/dev/shm/waybar-pokemon-$USER/frame.png`: the current frame (in memory, as it changes often)

## Credits

Names: [PokeAPI](https://pokeapi.co) ([data](https://github.com/PokeAPI/pokeapi)). Sprites: [PokéSprite](https://github.com/msikma/pokesprite) box icons, and [PokeAPI sprites](https://github.com/PokeAPI/sprites) for Pokémon without an icon there. They are downloaded at runtime and not part of this project. Pokémon and Pokémon character names are trademarks of Nintendo, Creatures Inc. and GAME FREAK inc.; this project is not affiliated with them.
