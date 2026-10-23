"""A Pokémon on the stage: a walker that sparkles if it is shiny, races
for berries and eats them."""

from dataclasses import dataclass

from PIL import Image

from ..engine import Stage, Walker
from .berry import BITES, Berry
from .heart import Heart
from .sparkle import Sparkle

RACE_SPEED = 30.0  # pixels per second, faster than walking
BITE = 0.6  # seconds per bite: two chews
CHEW = BITE / 4  # squashed for this long, twice per bite
SQUASH = 2  # pixels
JOY = 60.0  # speed of the happy jump after a meal, pixels per second


@dataclass(eq=False)
class Pokemon(Walker):
    sparkle: Sparkle | None = None  # only for shiny ones
    heart: Heart | None = None  # shown after a meal
    racing_for: Berry | None = None
    meal: Berry | None = None
    meal_clock: float = 0.0

    # Commands (from the habitat)

    def race_to(self, berry: Berry, stage: Stage) -> None:
        """Runs to the berry, so that the front of the sprite touches it."""
        self.racing_for = berry
        width = self.sprite.width
        if self.x + width / 2 < berry.center:
            target = berry.x - width + 2
        else:
            target = berry.x + berry.image.width - 2
        self.go_to(min(max(target, 0), max(0, stage.width - width)), RACE_SPEED)

    def give_up(self, stage: Stage) -> None:
        self.racing_for = None
        self.stop(stage)

    def eat(self, berry: Berry) -> None:
        berry.eater = self
        self.racing_for, self.meal, self.meal_clock = None, berry, 0.0
        self.state = "eat"
        self.direction = 1 if berry.center > self.x + self.sprite.width / 2 else -1

    # Update

    def update(self, dt: float, stage: Stage) -> None:
        if self.state == "eat":
            self._munch(dt, stage)
        else:
            super().update(dt, stage)
        if self.sparkle:
            self.sparkle.update(dt, stage.rng)
        if self.heart:
            self.heart.update(dt)
            if self.heart.done:
                self.heart = None

    def _munch(self, dt: float, stage: Stage) -> None:
        """A bite at the start of every BITE seconds; then a happy jump."""
        assert self.meal is not None
        self.clock += dt
        self.meal_clock += dt
        while self.meal.bites < min(BITES, int(self.meal_clock / BITE) + 1):
            self.meal.bite()
        if self.meal_clock >= BITES * BITE:
            self.meal = None
            self.heart = Heart()
            self.jump(JOY)

    @property
    def chewing(self) -> bool:
        """Squashed: in the first and third quarter of every bite."""
        return self.state == "eat" and int(self.meal_clock / CHEW) % 2 == 0

    # Draw

    @property
    def image(self) -> Image.Image:
        image = super().image
        if self.chewing and image.height > 2 * SQUASH:
            image = image.resize((image.width, image.height - SQUASH), Image.Resampling.NEAREST)
        return image

    def draw(self, frame: Image.Image, stage: Stage) -> None:
        super().draw(frame, stage)
        box = self.box(stage)
        if self.sparkle and self.state != "drop":
            self.sparkle.draw(frame, box)
        if self.heart:
            self.heart.draw(frame, box, self.direction)
