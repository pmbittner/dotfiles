"""An entity that drops in, stands around and walks along the ground."""

import math
from dataclasses import dataclass
from typing import Any

from PIL import Image

from .scene import Stage
from .sprite import Sprite

GRAVITY = 400.0  # pixels per second², for dropping onto the stage


@dataclass(eq=False)
class Walker:
    sprite: Sprite
    x: float  # left edge, in pixels
    direction: int  # -1 = left, 1 = right
    y: float = 0.0  # offset above the ground (negative = higher)
    velocity: float = 0.0  # vertical, while dropping
    speed: float = 12.0  # pixels per second while walking
    state: str = "drop"  # "drop", "idle" or "walk"
    timer: float = 0.0  # seconds left in the current state
    phase: float = 0.0  # step cycle while walking
    clock: float = 0.0  # seconds since appearing, for the jiggle while standing

    @classmethod
    def drop_in(cls, sprite: Sprite, stage: Stage, **fields: Any) -> "Walker":
        """A new walker that drops in at a random place from above.

        Subclasses can pass their own fields.
        """
        return cls(
            sprite=sprite,
            x=stage.rng.uniform(0, max(0, stage.width - sprite.width)),
            direction=stage.rng.choice((-1, 1)),
            y=-40.0,
            **fields,
        )

    # Update

    def update(self, dt: float, stage: Stage) -> None:
        self.clock += dt
        if self.state == "drop":
            self.velocity += GRAVITY * dt
            self.y = min(0.0, self.y + self.velocity * dt)
            if self.y >= 0:
                self._start_idle(stage)
            return

        self.timer -= dt
        if self.state == "idle":
            # Sometimes look the other way while standing.
            if stage.rng.random() < 0.3 * dt:
                self.direction = -self.direction
            if self.timer <= 0:
                self._start_walk(stage)
        else:
            self.x += self.direction * self.speed * dt
            self.phase += dt * self.speed / 8  # one step every 8 pixels
            right_edge = max(0, stage.width - self.sprite.width)
            if self.x <= 0 or self.x >= right_edge:
                self.x = min(max(self.x, 0), right_edge)
                self.direction = 1 if self.x <= 0 else -1  # turn around at walls
            if self.timer <= 0:
                self._start_idle(stage)

    def _start_idle(self, stage: Stage) -> None:
        self.state, self.timer, self.phase = "idle", stage.rng.uniform(1.0, 4.0), 0.0

    def _start_walk(self, stage: Stage) -> None:
        self.state, self.timer = "walk", stage.rng.uniform(1.5, 5.0)
        self.direction = stage.rng.choice((-1, 1))
        self.speed = stage.rng.uniform(8.0, 20.0)

    # Draw

    @property
    def hop(self) -> float:
        """Vertical offset of the animation, in pixels (negative = up).

        Walking: a small hop with every step. Standing: the jiggle of the
        party icons in the Pokémon games, one pixel up and down four times
        a second.
        """
        if self.state == "walk":
            return -abs(math.sin(math.pi * self.phase)) * 2
        if self.state == "idle":
            return -1.0 if int(self.clock * 4) % 2 else 0.0
        return 0.0

    @property
    def image(self) -> Image.Image:
        return self.sprite.right if self.direction > 0 else self.sprite.left

    def box(self, stage: Stage) -> tuple[int, int, int, int]:
        """Where the image is drawn: left, top, width, height."""
        image = self.image
        top = stage.height - image.height + round(self.y + self.hop)
        return round(self.x), top, image.width, image.height

    def draw(self, frame: Image.Image, stage: Stage) -> None:
        left, top, _, _ = self.box(stage)
        frame.paste(self.image, (left, top), self.image)
