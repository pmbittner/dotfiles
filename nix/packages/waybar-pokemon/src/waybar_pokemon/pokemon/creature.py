"""A Pokémon on the stage: a walker that sparkles if it is shiny."""

from dataclasses import dataclass

from PIL import Image

from ..engine import Stage, Walker
from .sparkle import Sparkle


@dataclass(eq=False)
class Pokemon(Walker):
    sparkle: Sparkle | None = None  # only for shiny ones

    def update(self, dt: float, stage: Stage) -> None:
        super().update(dt, stage)
        if self.sparkle:
            self.sparkle.update(dt, stage.rng)

    def draw(self, frame: Image.Image, stage: Stage) -> None:
        super().draw(frame, stage)
        if self.sparkle and self.state != "drop":
            self.sparkle.draw(frame, self.box(stage))
