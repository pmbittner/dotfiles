"""Stage, entities and scenes."""

import random
from dataclasses import dataclass, field
from typing import Hashable, Protocol

from PIL import Image


@dataclass
class Stage:
    """The area the entities live in, in pixels. y grows downwards; the
    ground is the bottom edge."""

    width: int
    height: int
    rng: random.Random = field(default_factory=random.Random)


class Entity(Protocol):
    """Anything on the stage that moves or changes over time."""

    def update(self, dt: float, stage: Stage) -> None:
        """Advances the entity by dt seconds."""

    def draw(self, frame: Image.Image, stage: Stage) -> None:
        """Paints the entity onto the frame, without changing its state."""


class Scene:
    """A stage and its entities, drawn in the order they were added.

    Subclasses decide which entities there are, in sync().
    """

    def __init__(self, stage: Stage) -> None:
        self.stage = stage
        self.entities: dict[Hashable, Entity] = {}

    def sync(self) -> None:
        """Input step: adds and removes entities, e.g. after a command."""

    def update(self, dt: float) -> None:
        for entity in self.entities.values():
            entity.update(dt, self.stage)

    def draw(self, frame: Image.Image) -> None:
        for entity in self.entities.values():
            entity.draw(frame, self.stage)
