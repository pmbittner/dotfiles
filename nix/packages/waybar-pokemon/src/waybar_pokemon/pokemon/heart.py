"""A little heart that rises and fades above a Pokémon, after a meal."""

from dataclasses import dataclass

from PIL import Image, ImageDraw

DURATION = 1.2  # seconds
RISE = 4  # pixels
COLOR = (255, 110, 150)
SHAPE = (
    ".XX.XX.",
    "XXXXXXX",
    "XXXXXXX",
    ".XXXXX.",
    "..XXX..",
    "...X...",
)


@dataclass
class Heart:
    clock: float = 0.0

    @property
    def done(self) -> bool:
        return self.clock >= DURATION

    def update(self, dt: float) -> None:
        self.clock += dt

    def draw(self, frame: Image.Image, box: tuple[int, int, int, int], direction: int) -> None:
        """Draws the heart above the front of the sprite drawn at box."""
        if self.done:
            return
        left, top, width, _ = box
        progress = self.clock / DURATION
        x = left + (width - len(SHAPE[0]) if direction > 0 else 0)
        y = max(0, top - len(SHAPE) + 2 - round(RISE * progress))
        alpha = 255 if progress < 0.6 else round(255 * (1 - progress) / 0.4)
        draw = ImageDraw.Draw(frame)
        for dy, row in enumerate(SHAPE):
            for dx, cell in enumerate(row):
                if cell == "X":
                    draw.point((x + dx, y + dy), fill=(*COLOR, alpha))
