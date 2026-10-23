"""Settings: built-in defaults, overridden by the config file.

Both the daemon and the commands read them.
"""

import json
from dataclasses import dataclass, field
from pathlib import Path

from . import paths


@dataclass
class Config:
    width: int = 240  # size of the habitat in pixels; waybar's "size" must match
    height: int = 30
    fps: int = 10  # frames per second; waybar's "interval" should be 1 / fps
    frame: str = ""  # path of the rendered frame; empty means the default
    # Download the sprites of all Pokémon (normal and shiny) in the
    # background, so any Pokémon can be spawned without internet. Otherwise a
    # sprite is downloaded when its Pokémon is spawned for the first time.
    cache_all_sprites: bool = False
    # Generations that random spawns pick from, e.g. [1, 2, 3, 4]. Empty
    # means all. Spawning a Pokémon by name is not restricted.
    random_generations: list[int] = field(default_factory=list)
    # How often a wild berry falls from the sky while there are Pokémon, on
    # average. 0 turns it off.
    wild_berries_per_hour: float = 1.0

    @property
    def frame_path(self) -> Path:
        return Path(self.frame) if self.frame else paths.default_frame_path()


def load(path: Path = paths.CONFIG_FILE) -> Config:
    """Reads the config file (a JSON object with any of the fields above)."""
    config = Config()
    if not path.exists():
        return config
    values = json.loads(path.read_text())
    for key, value in values.items():
        if not hasattr(config, key):
            raise ValueError(f"{path}: unknown setting '{key}'")
        default = getattr(config, key)
        if isinstance(default, list):
            value = [int(v) for v in value]
        elif isinstance(default, bool):
            if not isinstance(value, bool):
                raise ValueError(f"{path}: '{key}' must be true or false")
        else:
            value = type(default)(value)
        setattr(config, key, value)
    return config


def parse_generations(text: str) -> list[int]:
    """Parses generations like "1-4" or "1,3,5-6" (as pokemon-colorscripts)."""
    generations: list[int] = []
    for part in text.split(","):
        first, _, last = part.strip().partition("-")
        generations.extend(range(int(first), int(last or first) + 1))
    return generations
