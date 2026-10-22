"""How the Pokémon move: dropping in, standing around and walking."""

import math
import random
from dataclasses import dataclass

GRAVITY = 400.0  # pixels per second², for dropping into the habitat


@dataclass
class Walker:
    uid: int
    sprite_width: int
    x: float  # left edge, in pixels
    direction: int  # -1 = left, 1 = right
    shiny: bool = False
    y: float = 0.0  # offset above the ground (negative = higher)
    velocity: float = 0.0  # vertical, while dropping
    speed: float = 12.0  # pixels per second while walking
    state: str = "drop"  # "drop", "idle" or "walk"
    timer: float = 0.0  # seconds left in the current state
    phase: float = 0.0  # step cycle while walking
    clock: float = 0.0  # seconds since spawning, for the jiggle while standing

    @classmethod
    def spawn(cls, uid: int, shiny: bool, sprite_width: int, habitat_width: int, rng: random.Random) -> "Walker":
        """A new walker that drops in at a random place from above."""
        return cls(
            uid=uid,
            shiny=shiny,
            sprite_width=sprite_width,
            x=rng.uniform(0, max(0, habitat_width - sprite_width)),
            direction=rng.choice((-1, 1)),
            y=-40.0,
        )

    @property
    def hop(self) -> float:
        """Vertical offset of the animation, in pixels (negative = up).

        Walking: a small hop with every step. Standing: the jiggle of the
        party icons in the games, one pixel up and down four times a second.
        """
        if self.state == "walk":
            return -abs(math.sin(math.pi * self.phase)) * 2
        if self.state == "idle":
            return -1.0 if int(self.clock * 4) % 2 else 0.0
        return 0.0

    def step(self, dt: float, habitat_width: int, rng: random.Random) -> None:
        """Advances the walker by dt seconds."""
        self.clock += dt
        if self.state == "drop":
            self.velocity += GRAVITY * dt
            self.y = min(0.0, self.y + self.velocity * dt)
            if self.y >= 0:
                self._start_idle(rng)
            return

        self.timer -= dt
        if self.state == "idle":
            # Sometimes look the other way while standing.
            if rng.random() < 0.3 * dt:
                self.direction = -self.direction
            if self.timer <= 0:
                self._start_walk(rng)
        else:
            self.x += self.direction * self.speed * dt
            self.phase += dt * self.speed / 8  # one step every 8 pixels
            right_edge = max(0, habitat_width - self.sprite_width)
            if self.x <= 0 or self.x >= right_edge:
                self.x = min(max(self.x, 0), right_edge)
                self.direction = 1 if self.x <= 0 else -1  # turn around at walls
            if self.timer <= 0:
                self._start_idle(rng)

    def _start_idle(self, rng: random.Random) -> None:
        self.state, self.timer, self.phase = "idle", rng.uniform(1.0, 4.0), 0.0

    def _start_walk(self, rng: random.Random) -> None:
        self.state, self.timer = "walk", rng.uniform(1.5, 5.0)
        self.direction = rng.choice((-1, 1))
        self.speed = rng.uniform(8.0, 20.0)
