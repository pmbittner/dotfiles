"""The daemon: shows the party's Pokémon and writes the frames for the bar."""

import sys
import threading

from . import engine
from .config import Config
from .pokemon import pokedex
from .pokemon.habitat import Habitat


def run(config: Config) -> None:
    if config.cache_all_sprites:
        threading.Thread(target=_cache_all_sprites, daemon=True).start()
    habitat = Habitat(engine.Stage(config.width, config.height))
    engine.run(habitat, engine.PngFile(config.frame_path), config.fps)


def _cache_all_sprites() -> None:
    """Downloads the missing sprites (see Config.cache_all_sprites)."""
    try:
        failed = pokedex.download_all()
    except OSError as error:  # e.g. the species list cannot be downloaded
        print(f"waybar-pokemon: cannot cache the sprites: {error}", file=sys.stderr)
        return
    if failed:
        print(f"waybar-pokemon: {failed} sprites could not be downloaded; retrying on the next start", file=sys.stderr)
