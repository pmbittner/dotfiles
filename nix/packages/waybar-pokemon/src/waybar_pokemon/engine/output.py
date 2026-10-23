"""Outputs present the frames to the user, e.g. as a file that a bar shows."""

import os
from pathlib import Path
from typing import Protocol

from PIL import Image


class Output(Protocol):
    def present(self, frame: Image.Image) -> None: ...


class PngFile:
    """Writes the frames to a PNG file, for bars that show an image file
    and read it again in a short interval (e.g. waybar's image module).

    The file is only rewritten when the picture changed, and replaced
    atomically, so a reader never sees half a frame.
    """

    def __init__(self, path: Path) -> None:
        self.path = path
        self._temporary = path.with_name("." + path.name)
        self._last: bytes | None = None
        path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)

    def present(self, frame: Image.Image) -> None:
        pixels = frame.tobytes()
        if pixels == self._last:
            return
        self._last = pixels
        frame.save(self._temporary, format="PNG", compress_level=1)
        os.replace(self._temporary, self.path)
