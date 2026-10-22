"""The daemon: simulates the party and writes a new frame for waybar.

waybar's image module reads the frame file in a fixed interval. The daemon
only rewrites it when the picture changed.
"""

import os
import random
import sys
import threading
import time

from . import party, paths, pokedex
from .config import Config
from .render import Sprite, load_sprite, render
from .simulation import Walker


def run(config: Config) -> None:
    frame_path = config.frame_path
    frame_path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    if config.cache_all_sprites:
        threading.Thread(target=_cache_all_sprites, daemon=True).start()
    temporary = frame_path.with_name("." + frame_path.name)

    rng = random.Random()
    walkers: dict[int, Walker] = {}
    sprites: dict[int, Sprite] = {}
    party_version = None
    last_frame = None
    period = 1.0 / config.fps
    last = time.monotonic()

    while True:
        now = time.monotonic()
        dt, last = min(now - last, 0.5), now

        # Follow changes of the party file (spawn and despawn commands).
        version = _modification_time(paths.PARTY_FILE)
        if version != party_version:
            party_version = version
            _sync(walkers, sprites, party.load(), config, rng)

        for walker in walkers.values():
            walker.step(dt, config.width, rng)

        frame = render(list(walkers.values()), sprites, config.width, config.height, now)
        pixels = frame.tobytes()
        if pixels != last_frame:
            last_frame = pixels
            frame.save(temporary, format="PNG", compress_level=1)
            os.replace(temporary, frame_path)

        time.sleep(max(0.0, period - (time.monotonic() - now)))


def _cache_all_sprites() -> None:
    """Downloads the missing sprites (see Config.cache_all_sprites)."""
    try:
        failed = pokedex.download_all()
    except OSError as error:  # e.g. the species list cannot be downloaded
        print(f"waybar-pokemon: cannot cache the sprites: {error}", file=sys.stderr)
        return
    if failed:
        print(f"waybar-pokemon: {failed} sprites could not be downloaded; retrying on the next start", file=sys.stderr)


def _modification_time(path) -> int | None:
    try:
        return path.stat().st_mtime_ns
    except FileNotFoundError:
        return None


def _sync(walkers: dict[int, Walker], sprites: dict[int, Sprite], members: list[party.Member], config: Config, rng: random.Random) -> None:
    """Adds walkers for new party members and removes the ones that left."""
    uids = {m.uid for m in members}
    for uid in list(walkers):
        if uid not in uids:
            del walkers[uid], sprites[uid]
    for member in members:
        if member.uid in walkers:
            continue
        try:
            sprite = load_sprite(pokedex.sprite_path(member.species_id, member.shiny), config.height - 2)
        except Exception as error:  # e.g. no network for a missing sprite
            print(f"waybar-pokemon: cannot load {member.name}: {error}", file=sys.stderr)
            continue
        sprites[member.uid] = sprite
        walkers[member.uid] = Walker.spawn(member.uid, member.shiny, sprite[0].width, config.width, rng)
