"""The sign of a shiny Pokémon: a little star twinkling on its sprite."""

import random
from dataclasses import dataclass

from PIL import Image, ImageDraw

PERIOD = 2.0  # seconds from one twinkle to the next
DURATION = 0.5  # seconds a twinkle lasts


@dataclass
class Sparkle:
    clock: float = 0.0  # seconds into the current period
    spot: tuple[float, float] = (0.5, 0.25)  # on the sprite, relative to its size

    @classmethod
    def start(cls, rng: random.Random) -> "Sparkle":
        """Starts at a random moment, so shiny Pokémon don't twinkle in sync."""
        return cls(clock=rng.uniform(0, PERIOD))

    def update(self, dt: float, rng: random.Random) -> None:
        self.clock += dt
        if self.clock >= PERIOD:
            self.clock %= PERIOD
            # A new spot every time, in the upper half of the sprite.
            self.spot = (rng.random(), rng.random() / 2)

    @property
    def size(self) -> int:
        """Length of the star's rays: growing, full, shrinking, then gone."""
        if self.clock >= DURATION:
            return 0
        return 2 if 0.3 * DURATION <= self.clock <= 0.7 * DURATION else 1

    def draw(self, frame: Image.Image, box: tuple[int, int, int, int]) -> None:
        """Draws the star on the sprite drawn at box (left, top, width, height)."""
        size = self.size
        if not size:
            return
        left, top, width, height = box
        x = left + int(self.spot[0] * width)
        y = top + int(self.spot[1] * height)
        draw = ImageDraw.Draw(frame)
        draw.point((x, y), fill=(255, 255, 255, 255))
        for distance in range(1, size + 1):
            alpha = 255 - 70 * distance
            for dx, dy in ((distance, 0), (-distance, 0), (0, distance), (0, -distance)):
                draw.point((x + dx, y + dy), fill=(255, 240, 150, alpha))
