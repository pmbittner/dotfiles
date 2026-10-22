"""Pokémon names and sprites, downloaded from the PokeAPI project and cached.

Names come from PokeAPI's species table, sprites from PokeAPI's sprite
collection (front sprites, normal and shiny). Both are fetched from GitHub
on first use and kept in the cache directory.
"""

import csv
import random
import urllib.request
from dataclasses import dataclass
from pathlib import Path

from . import paths

SPECIES_URL = "https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon_species.csv"
SPRITE_URL = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/{variant}{id}.png"

# The chance of a shiny Pokémon in the games since generation 6.
SHINY_ODDS = 4096


@dataclass(frozen=True)
class Species:
    id: int
    name: str  # e.g. "mr-mime"

    @property
    def display_name(self) -> str:
        return self.name.replace("-", " ").title()


def _download(url: str, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    request = urllib.request.Request(url, headers={"User-Agent": "waybar-pokemon"})
    with urllib.request.urlopen(request, timeout=30) as response:
        data = response.read()
    temporary = destination.with_suffix(destination.suffix + ".part")
    temporary.write_bytes(data)
    temporary.replace(destination)


def all_species() -> list[Species]:
    """All species, ordered by Pokédex number."""
    if not paths.SPECIES_FILE.exists():
        _download(SPECIES_URL, paths.SPECIES_FILE)
    with paths.SPECIES_FILE.open(newline="") as file:
        rows = csv.DictReader(file)
        species = [Species(int(row["id"]), row["identifier"]) for row in rows]
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


def random_species() -> Species:
    return random.choice(all_species())


def roll_shiny() -> bool:
    return random.randrange(SHINY_ODDS) == 0


def sprite_path(species_id: int, shiny: bool) -> Path:
    """The cached sprite, downloaded if missing."""
    name = f"{species_id}{'-shiny' if shiny else ''}.png"
    path = paths.SPRITE_DIR / name
    if not path.exists():
        _download(SPRITE_URL.format(variant="shiny/" if shiny else "", id=species_id), path)
    return path
