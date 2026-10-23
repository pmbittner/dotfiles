"""A berry on the stage: it drops in, lies on the ground and gets eaten
in a few bites."""

from dataclasses import dataclass

from PIL import Image

from ..engine import Stage
from ..engine.walker import GRAVITY

BITES = 3


@dataclass(eq=False)
class Berry:
    image: Image.Image
    x: float  # left edge, in pixels
    y: float = -40.0  # offset above the ground (negative = higher)
    velocity: float = 0.0
    bites: int = 0  # taken so far
    eater: object | None = None  # the Pokémon that claimed it

    @classmethod
    def drop_in(cls, image: Image.Image, stage: Stage) -> "Berry":
        return cls(image=image, x=stage.rng.uniform(0, max(0, stage.width - image.width)))

    @property
    def landed(self) -> bool:
        return self.y >= 0

    @property
    def eaten(self) -> bool:
        return self.bites >= BITES

    @property
    def center(self) -> float:
        return self.x + self.image.width / 2

    def bite(self) -> None:
        self.bites = min(BITES, self.bites + 1)

    def update(self, dt: float, stage: Stage) -> None:
        if not self.landed:
            self.velocity += GRAVITY * dt
            self.y = min(0.0, self.y + self.velocity * dt)

    def draw(self, frame: Image.Image, stage: Stage) -> None:
        """Smaller with every bite, standing on the ground."""
        if self.eaten:
            return
        image = self.image
        if self.bites:
            left = (BITES - self.bites) / BITES
            size = (max(1, round(image.width * left)), max(1, round(image.height * left)))
            image = image.resize(size, Image.Resampling.NEAREST)
        x = round(self.center - image.width / 2)
        top = stage.height - image.height + round(self.y)
        frame.paste(image, (x, top), image)
