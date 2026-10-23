"""Pokémon names and sprites, downloaded on first use and cached.

Names come from PokeAPI's species table. Sprites are the small box icons
of the games (68 x 56, normal and shiny) from the PokéSprite project: the
classic icons of generation 7 where there is one (Pokémon up to generation
7), else the icons of generation 8. For Pokémon without any icon there (the
newest ones), PokeAPI's larger front sprites are used instead. Everything is
fetched from GitHub.
"""

import csv
import os
import random
import threading
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from functools import cache, partial
from pathlib import Path
from typing import Callable, Collection

from .. import paths

SPECIES_URL = "https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon_species.csv"
ICON_URL = "https://raw.githubusercontent.com/msikma/pokesprite/master/pokemon-{icon_set}/{variant}/{name}.png"
ICON_SETS = ("gen7x", "gen8")  # tried in this order
SPRITE_URL = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/{variant}{id}.png"

# The chance of a shiny Pokémon in the games since generation 6.
SHINY_ODDS = 4096


@dataclass(frozen=True)
class Species:
    id: int
    name: str  # e.g. "mr-mime"
    generation: int

    @property
    def display_name(self) -> str:
        return self.name.replace("-", " ").title()


def download(url: str, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    request = urllib.request.Request(url, headers={"User-Agent": "waybar-pokemon"})
    with urllib.request.urlopen(request, timeout=30) as response:
        data = response.read()
    # A unique temporary name, as the daemon may download in the background.
    temporary = destination.with_name(f".{destination.name}.{os.getpid()}.{threading.get_ident()}.part")
    temporary.write_bytes(data)
    temporary.replace(destination)


@cache
def all_species() -> list[Species]:
    """All species, ordered by Pokédex number (read once per process)."""
    if not paths.SPECIES_FILE.exists():
        download(SPECIES_URL, paths.SPECIES_FILE)
    with paths.SPECIES_FILE.open(newline="") as file:
        rows = csv.DictReader(file)
        species = [Species(int(row["id"]), row["identifier"], int(row["generation_id"])) for row in rows]
    return sorted(species, key=lambda s: s.id)


def find(query: str) -> Species:
    """Looks up a species by Pokédex number or (case-insensitive) name.

    A unique beginning of a name is enough, e.g. "pika".
    """
    species = all_species()
    if query.isdigit():
        matches = [s for s in species if s.id == int(query)]
    else:
        key = query.strip().lower().replace(" ", "-")
        matches = [s for s in species if s.name == key] or [
            s for s in species if s.name.startswith(key)
        ]
    if len(matches) != 1:
        found = ", ".join(s.display_name for s in matches[:5])
        raise LookupError(f"'{query}' matches {len(matches)} Pokémon" + (f": {found}" if found else ""))
    return matches[0]


def random_species(generations: Collection[int] = ()) -> Species:
    """A random species, only from the given generations if any are given."""
    candidates = [s for s in all_species() if not generations or s.generation in generations]
    if not candidates:
        raise LookupError(f"No Pokémon in generations {sorted(generations)}")
    return random.choice(candidates)


def roll_shiny() -> bool:
    return random.randrange(SHINY_ODDS) == 0


def sprite_path(species_id: int, shiny: bool) -> Path:
    """The cached sprite (box icon, or front sprite as fallback), downloaded if missing."""
    # One directory per choice of icons, so changing ICON_SETS downloads
    # the new icons instead of reusing the cached ones.
    path = paths.SPRITE_DIR / "+".join(ICON_SETS) / f"{species_id}{'-shiny' if shiny else ''}.png"
    if path.exists():
        return path
    name = next(s.name for s in all_species() if s.id == species_id)
    variant = "shiny" if shiny else "regular"
    urls = [ICON_URL.format(icon_set=icon_set, variant=variant, name=name) for icon_set in ICON_SETS]
    urls.append(SPRITE_URL.format(variant="shiny/" if shiny else "", id=species_id))
    for url in urls[:-1]:
        try:
            download(url, path)
            return path
        except urllib.error.HTTPError as error:
            if error.code != 404:
                raise
    download(urls[-1], path)
    return path


def download_all(progress: Callable[[int, int], None] | None = None) -> int:
    """Downloads the sprites of all species (normal and shiny) and of all
    berries that are not cached yet. Returns the number of sprites that
    could not be downloaded.
    """
    from . import berries  # (berries uses this module)

    jobs: list[Callable[[], Path]] = [partial(sprite_path, s.id, shiny) for s in all_species() for shiny in (False, True)]
    jobs += [partial(berries.sprite_path, name) for name in berries.NAMES]
    failed = 0
    # A few downloads at a time; one by one takes several minutes.
    with ThreadPoolExecutor(max_workers=8) as pool:
        futures = [pool.submit(job) for job in jobs]
        for done, future in enumerate(as_completed(futures), start=1):
            if isinstance(future.exception(), OSError):
                failed += 1
            if progress:
                progress(done, len(jobs))
    return failed
