"""Settings of the daemon: built-in defaults, overridden by the config file."""

import json
from dataclasses import dataclass, fields
from pathlib import Path

from . import paths


@dataclass
class Config:
    width: int = 240  # size of the habitat in pixels; waybar's "size" must match
    height: int = 30
    fps: int = 10  # frames per second; waybar's "interval" should be 1 / fps
    frame: str = ""  # path of the rendered frame; empty means the default

    @property
    def frame_path(self) -> Path:
        return Path(self.frame) if self.frame else paths.default_frame_path()


def load(path: Path = paths.CONFIG_FILE) -> Config:
    """Reads the config file (JSON with any of the fields above), if it exists."""
    config = Config()
    if path.exists():
        values = json.loads(path.read_text())
        for field in fields(Config):
            if field.name in values:
                default = getattr(config, field.name)
                setattr(config, field.name, type(default)(values[field.name]))
    return config
