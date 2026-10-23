"""Sprites: images facing left and right."""

from dataclasses import dataclass
from pathlib import Path

from PIL import Image


@dataclass(frozen=True)
class Sprite:
    left: Image.Image
    right: Image.Image

    @property
    def width(self) -> int:
        return self.left.width


def load_sprite(path: Path, max_height: int) -> Sprite:
    """Loads an image facing left, crops its empty border and shrinks it if
    it is too tall.

    Sprites are never enlarged, so small ones stay small and their sizes
    keep their proportions to each other as long as they fit.
    """
    image = Image.open(path).convert("RGBA")
    box = image.getbbox()
    if box:
        image = image.crop(box)
    if image.height > max_height:
        width = max(1, round(image.width * max_height / image.height))
        image = image.resize((width, max_height), Image.Resampling.LANCZOS)
    return Sprite(left=image, right=image.transpose(Image.Transpose.FLIP_LEFT_RIGHT))
