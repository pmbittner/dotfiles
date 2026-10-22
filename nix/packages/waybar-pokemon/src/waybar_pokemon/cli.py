"""Command line: run the daemon, spawn and despawn Pokémon."""

import argparse
import shlex
import shutil
import subprocess
import sys

from . import config, daemon, party, pokedex

DEFAULT_MENU = "rofi -dmenu -i -p Pokémon"
RANDOM = "Random"
EVERYONE = "Everyone"


def notify(enabled: bool, message: str) -> None:
    if enabled and shutil.which("notify-send"):
        subprocess.run(["notify-send", "Pokémon", message], check=False)


def fail(args: argparse.Namespace, message: str) -> None:
    print(f"waybar-pokemon: {message}", file=sys.stderr)
    notify(getattr(args, "notify", False), message)
    sys.exit(1)


def random_generations(args: argparse.Namespace) -> list[int]:
    """Generations for random spawns: --generations, else the config file."""
    if getattr(args, "generations", None):
        return config.parse_generations(args.generations)
    return config.load().random_generations


def spawn(args: argparse.Namespace, query: str | None) -> None:
    try:
        species = pokedex.find(query) if query else pokedex.random_species(random_generations(args))
        shiny = args.shiny or pokedex.roll_shiny()
        pokedex.sprite_path(species.id, shiny)  # download now, so errors show here
    except (LookupError, OSError, ValueError) as error:
        fail(args, f"{error} (no internet? See cache_all_sprites in the README.)" if isinstance(error, OSError) else str(error))
    with party.editing() as members:
        if len(members) >= party.MAX_SIZE:
            fail(args, f"The habitat is full ({party.MAX_SIZE} Pokémon). Despawn one first.")
        member = party.Member(party.next_uid(members), species.id, species.display_name, shiny)
        members.append(member)
    if shiny:
        notify(args.notify, f"✨ A shiny {member.name} appeared! ✨")
    else:
        notify(args.notify, f"A wild {member.name} appeared!")
    print(member.label)


def despawn(args: argparse.Namespace, query: str | None, everyone: bool = False) -> None:
    """Removes everyone, the member with the given uid or name, or the newest one."""
    with party.editing() as members:
        if not members:
            fail(args, "No Pokémon to despawn.")
        if everyone:
            leaving = list(members)
        elif query is None:
            leaving = [members[-1]]
        elif query.isdigit() and any(m.uid == int(query) for m in members):
            leaving = [m for m in members if m.uid == int(query)]
        else:
            matching = [m for m in members if m.name.lower().startswith(query.lower())]
            if not matching:
                fail(args, f"No '{query}' in the habitat.")
            leaving = [matching[-1]]
        for member in leaving:
            members.remove(member)
    names = ", ".join(m.name for m in leaving)
    notify(args.notify, f"{names} returned to {'their' if len(leaving) > 1 else 'its'} Poké Ball.")


def download() -> None:
    def progress(done: int, total: int) -> None:
        print(f"\r{done}/{total} sprites", end="", file=sys.stderr, flush=True)

    try:
        failed = pokedex.download_all(progress)
    except OSError as error:
        print(f"\nwaybar-pokemon: {error}", file=sys.stderr)
        sys.exit(1)
    print(file=sys.stderr)
    if failed:
        print(f"{failed} sprites could not be downloaded.", file=sys.stderr)
        sys.exit(1)


def choose(menu_command: str, options: list[str]) -> str | None:
    """Lets the user pick one of the options with a dmenu-like program."""
    result = subprocess.run(shlex.split(menu_command), input="\n".join(options), capture_output=True, text=True)
    choice = result.stdout.strip()
    return choice if result.returncode == 0 and choice else None


def spawn_menu(args: argparse.Namespace) -> None:
    try:
        names = [s.display_name for s in pokedex.all_species()]
    except OSError as error:
        fail(args, str(error))
    try:
        generations = random_generations(args)
    except ValueError as error:
        fail(args, str(error))
    random_label = f"{RANDOM} (generation {','.join(map(str, generations))})" if generations else RANDOM
    choice = choose(args.menu, [random_label] + names)
    if choice:
        spawn(args, None if choice == random_label else choice)


def despawn_menu(args: argparse.Namespace) -> None:
    members = party.load()
    if not members:
        fail(args, "No Pokémon to despawn.")
    options = [f"{m.uid}: {m.label}" for m in members] + [EVERYONE]
    choice = choose(args.menu, options)
    if choice == EVERYONE:
        despawn(args, None, everyone=True)
    elif choice:
        despawn(args, choice.split(":", 1)[0])


def main() -> None:
    parser = argparse.ArgumentParser(prog="waybar-pokemon", description="Little Pokémon walking around in waybar.")
    commands = parser.add_subparsers(dest="command", required=True)

    commands.add_parser("daemon", help="simulate the Pokémon and render the frames for waybar")

    p = commands.add_parser("spawn", help="send out a Pokémon (a random one without a name)")
    p.add_argument("pokemon", nargs="?", help="name (or its beginning) or Pokédex number")
    p.add_argument("--shiny", action="store_true", help="make it shiny (otherwise 1 in 4096)")

    p = commands.add_parser("despawn", help="return a Pokémon (the newest one without a name)")
    p.add_argument("pokemon", nargs="?", help="name or number from `list`")
    p.add_argument("--all", action="store_true", help="return everyone")

    commands.add_parser("list", help="show the party: number and name")
    commands.add_parser("download", help="download all sprites now, for spawning without internet")
    commands.add_parser("names", help="show all Pokémon names")

    p = commands.add_parser("spawn-menu", help="choose a Pokémon to spawn from a menu")
    p.add_argument("--shiny", action="store_true", help=argparse.SUPPRESS)
    commands.add_parser("despawn-menu", help="choose a Pokémon to despawn from a menu")

    for name, sub in commands.choices.items():
        if name not in ("daemon", "list", "names"):
            sub.add_argument("--notify", action="store_true", help="show the result as a desktop notification")
        if name.endswith("-menu"):
            sub.add_argument("--menu", default=DEFAULT_MENU, help=f"dmenu-like command (default: {DEFAULT_MENU})")
        if name in ("spawn", "spawn-menu"):
            sub.add_argument("--generations", help='random ones only from these generations, e.g. "1-4" (default: from the config file)')

    args = parser.parse_args()
    if args.command == "daemon":
        daemon.run(config.load())
    elif args.command == "spawn":
        spawn(args, args.pokemon)
    elif args.command == "despawn":
        despawn(args, args.pokemon, everyone=args.all)
    elif args.command == "list":
        for member in party.load():
            print(f"{member.uid}: {member.label}")
    elif args.command == "download":
        download()
    elif args.command == "names":
        print("\n".join(s.display_name for s in pokedex.all_species()))
    elif args.command == "spawn-menu":
        spawn_menu(args)
    elif args.command == "despawn-menu":
        despawn_menu(args)
