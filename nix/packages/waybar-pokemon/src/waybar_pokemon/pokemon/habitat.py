"""The habitat: a scene with the Pokémon of the party.

It follows the party file, which the spawn and despawn commands change.
"""

import sys

from .. import paths
from ..engine import Scene, Stage, load_sprite
from . import party, pokedex
from .creature import Pokemon
from .sparkle import Sparkle


class Habitat(Scene):
    def __init__(self, stage: Stage) -> None:
        super().__init__(stage)
        self._party_version: int | None = None

    def sync(self) -> None:
        version = _modification_time()
        if version == self._party_version:
            return
        self._party_version = version

        members = party.load()
        uids = {m.uid for m in members}
        for uid in list(self.entities):
            if uid not in uids:
                del self.entities[uid]
        for member in members:
            if member.uid not in self.entities:
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
        self.entities[member.uid] = Pokemon.drop_in(sprite, self.stage, sparkle=sparkle)


def _modification_time() -> int | None:
    try:
        return paths.PARTY_FILE.stat().st_mtime_ns
    except FileNotFoundError:
        return None
