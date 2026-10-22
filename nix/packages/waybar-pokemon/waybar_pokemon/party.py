"""The Pokémon currently out in the habitat, stored in a JSON file.

The command line adds and removes members; the daemon watches the file and
shows its members. A lock file keeps concurrent commands from losing
changes.
"""

import fcntl
import json
from contextlib import contextmanager
from dataclasses import asdict, dataclass
from typing import Iterator

from . import paths

MAX_SIZE = 10  # more would not fit into a small habitat


@dataclass(frozen=True)
class Member:
    uid: int  # unique within the party, to tell apart two of the same species
    species_id: int
    name: str
    shiny: bool

    @property
    def label(self) -> str:
        return f"{self.name}{' ✨' if self.shiny else ''}"


def load() -> list[Member]:
    try:
        data = json.loads(paths.PARTY_FILE.read_text())
    except (FileNotFoundError, json.JSONDecodeError):
        return []
    return [Member(**member) for member in data]


def _save(members: list[Member]) -> None:
    temporary = paths.PARTY_FILE.with_suffix(".json.part")
    temporary.write_text(json.dumps([asdict(m) for m in members], indent=2))
    temporary.replace(paths.PARTY_FILE)


@contextmanager
def editing() -> Iterator[list[Member]]:
    """Yields the party as a list to change; saves it afterwards."""
    paths.STATE_DIR.mkdir(parents=True, exist_ok=True)
    with (paths.STATE_DIR / "party.lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        members = load()
        yield members
        _save(members)


def next_uid(members: list[Member]) -> int:
    return max((m.uid for m in members), default=0) + 1
