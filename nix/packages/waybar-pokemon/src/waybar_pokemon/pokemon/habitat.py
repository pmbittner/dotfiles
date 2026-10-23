"""The habitat: a scene with the Pokémon of the party and berries.

It follows the party file, which the spawn and despawn commands change,
and drops the berries that the berry command requests. Now and then, a
wild berry falls from the sky.

When a berry has landed, every Pokémon on the ground that sees it races
for it. The first one there eats it; the others give up.
"""

import itertools
import sys
from typing import Iterable

from .. import paths
from ..engine import Entity, Scene, Stage, load_sprite
from . import berries, party, pokedex
from .berry import Berry
from .creature import Pokemon
from .sparkle import Sparkle

SIGHT = 80  # pixels: how far a Pokémon sees a berry (center to center)
MAX_BERRIES = 3  # more requests are ignored, so clicking fast cannot flood the habitat


class Habitat(Scene):
    def __init__(self, stage: Stage, wild_berries_per_hour: float = 1.0) -> None:
        super().__init__(stage)
        self.pokemon: dict[int, Pokemon] = {}  # by party uid
        self.berries: list[Berry] = []
        self.wild_berries_per_hour = wild_berries_per_hour
        self._party_version: int | None = None

    def entities(self) -> Iterable[Entity]:
        return itertools.chain(self.berries, self.pokemon.values())  # berries behind

    # Input

    def sync(self) -> None:
        self._sync_party()
        for name in berries.take_requests():
            self.drop_berry(name)

    def _sync_party(self) -> None:
        version = _modification_time()
        if version == self._party_version:
            return
        self._party_version = version

        members = party.load()
        uids = {m.uid for m in members}
        for uid in list(self.pokemon):
            if uid not in uids:
                del self.pokemon[uid]
        for member in members:
            if member.uid not in self.pokemon:
                self._add(member)

    def _add(self, member: party.Member) -> None:
        try:
            # A little smaller than the habitat, so hopping stays inside.
            path = pokedex.sprite_path(member.species_id, member.shiny)
            sprite = load_sprite(path, self.stage.height - 2)
        except Exception as error:  # e.g. no network for a missing sprite
            print(f"waybar-pokemon: cannot load {member.name}: {error}", file=sys.stderr)
            return
        sparkle = Sparkle.start(self.stage.rng) if member.shiny else None
        self.pokemon[member.uid] = Pokemon.drop_in(sprite, self.stage, sparkle=sparkle)

    def drop_berry(self, name: str) -> None:
        if len(self.berries) >= MAX_BERRIES:
            return
        try:
            image = load_sprite(berries.sprite_path(name), self.stage.height - 2).left
        except Exception as error:
            print(f"waybar-pokemon: cannot load the {berries.display_name(name)}: {error}", file=sys.stderr)
            return
        self.berries.append(Berry.drop_in(image, self.stage))

    # Update

    def update(self, dt: float) -> None:
        rng = self.stage.rng
        if self.pokemon and rng.random() < self.wild_berries_per_hour * dt / 3600:
            self.drop_berry(berries.random_name(rng))
        super().update(dt)
        self._feed()

    def _feed(self) -> None:
        """The race for the berries."""
        self.berries = [b for b in self.berries if not b.eaten]
        pokemon = list(self.pokemon.values())
        for berry in self.berries:
            if berry.eater is not None and berry.eater not in pokemon:
                berry.eater = None  # despawned while eating; free again
            if berry.eater is not None or not berry.landed:
                continue
            racers = [p for p in pokemon if p.racing_for is berry]
            winners = [p for p in racers if p.arrived]
            if winners:
                winner = min(winners, key=lambda p: abs(p.x + p.sprite.width / 2 - berry.center))
                winner.eat(berry)
                for loser in racers:
                    if loser is not winner:
                        loser.give_up(self.stage)
                continue
            for p in pokemon:
                if p.grounded and abs(p.x + p.sprite.width / 2 - berry.center) <= SIGHT:
                    p.race_to(berry, self.stage)
        # Racers whose berry is gone give up.
        for p in pokemon:
            if p.racing_for is not None and p.racing_for not in self.berries:
                p.give_up(self.stage)


def _modification_time() -> int | None:
    try:
        return paths.PARTY_FILE.stat().st_mtime_ns
    except FileNotFoundError:
        return None
