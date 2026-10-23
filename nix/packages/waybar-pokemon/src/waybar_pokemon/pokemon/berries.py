"""Berries: their names and sprites, and the requests to drop one.

The berry command adds a request to a small file; the daemon takes the
requests and drops the berries. Unlike the party, berries are not kept
across restarts.
"""

import fcntl
import random
from contextlib import contextmanager
from pathlib import Path
from typing import Iterator

from .. import paths
from . import pokedex

# The bag icons of the games, from PokéSprite (32 x 32).
SPRITE_URL = "https://raw.githubusercontent.com/msikma/pokesprite/master/items/berry/{name}.png"

NAMES = (
    "aguav", "apicot", "aspear", "babiri", "belue", "bluk", "charti", "cheri", "chesto", "chilan",
    "chople", "coba", "colbur", "cornn", "custap", "durin", "enigma", "figy", "ganlon",
    "golden-nanab", "golden-pinap", "golden-razz", "grepa", "haban", "hondew", "iapapa", "jaboca",
    "kasib", "kebia", "kee", "kelpsy", "lansat", "leppa", "liechi", "lum", "mago", "magost",
    "maranga", "micle", "nanab", "nomel", "occa", "oran", "pamtre", "passho", "payapa", "pecha",
    "persim", "petaya", "pinap", "pomeg", "qualot", "rabuta", "rawst", "razz", "rindo", "roseli",
    "rowap", "salac", "shuca", "silver-nanab", "silver-pinap", "silver-razz", "sitrus", "spelon",
    "starf", "tamato", "tanga", "wacan", "watmel", "wepear", "wiki", "yache",
)


def display_name(name: str) -> str:
    return name.replace("-", " ").title() + " Berry"


def find(query: str) -> str:
    """A berry by its name or a unique beginning of it, e.g. "sit" or "Oran Berry"."""
    key = query.strip().lower().removesuffix("berry").strip().replace(" ", "-")
    matches = [n for n in NAMES if n == key] or [n for n in NAMES if n.startswith(key)]
    if len(matches) != 1:
        found = ", ".join(display_name(n) for n in matches[:5])
        raise LookupError(f"'{query}' matches {len(matches)} berries" + (f": {found}" if found else ""))
    return matches[0]


def random_name(rng: random.Random | None = None) -> str:
    return (rng or random).choice(NAMES)


def sprite_path(name: str) -> Path:
    """The cached sprite of a berry, downloaded if missing."""
    path = paths.SPRITE_DIR / "berries" / f"{name}.png"
    if not path.exists():
        pokedex.download(SPRITE_URL.format(name=name), path)
    return path


@contextmanager
def _locked() -> Iterator[None]:
    paths.STATE_DIR.mkdir(parents=True, exist_ok=True)
    with (paths.STATE_DIR / "berries.lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        yield


def request(name: str) -> None:
    """Asks the daemon to drop a berry."""
    with _locked(), paths.BERRY_REQUESTS.open("a") as file:
        file.write(name + "\n")


def take_requests() -> list[str]:
    """The berries to drop since the last call (for the daemon)."""
    if not paths.BERRY_REQUESTS.exists():  # the usual case, without locking
        return []
    with _locked():
        try:
            lines = paths.BERRY_REQUESTS.read_text().split()
            paths.BERRY_REQUESTS.unlink()
        except FileNotFoundError:
            return []
    return [line for line in lines if line in NAMES]
