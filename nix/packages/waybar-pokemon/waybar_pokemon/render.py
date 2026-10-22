"""Draws the habitat: all walkers on a transparent strip."""

from pathlib import Path

from PIL import Image, ImageDraw

from .simulation import Walker

# Sprites (box icons and front sprites) face left; the mirrored copy is used
# when walking right.
Sprite = tuple[Image.Image, Image.Image]


def load_sprite(path: Path, max_height: int) -> Sprite:
    """Crops the empty border of a sprite and shrinks it if it is too tall.

    Small Pokémon stay small: sprites are never enlarged, so their sizes
    keep their proportions to each other as long as they fit.
    """
    image = Image.open(path).convert("RGBA")
    box = image.getbbox()
    if box:
        image = image.crop(box)
    if image.height > max_height:
        width = max(1, round(image.width * max_height / image.height))
        image = image.resize((width, max_height), Image.Resampling.LANCZOS)
    return image, image.transpose(Image.Transpose.FLIP_LEFT_RIGHT)


def _sparkle(draw: ImageDraw.ImageDraw, x: int, y: int, size: int) -> None:
    """A small four-pointed star, the sign of a shiny Pokémon."""
    draw.point((x, y), fill=(255, 255, 255, 255))
    for distance in range(1, size + 1):
        alpha = 255 - 70 * distance
        for dx, dy in ((distance, 0), (-distance, 0), (0, distance), (0, -distance)):
            draw.point((x + dx, y + dy), fill=(255, 240, 150, alpha))


def render(walkers: list[Walker], sprites: dict[int, Sprite], width: int, height: int, time: float) -> Image.Image:
    frame = Image.new("RGBA", (width, height), (0, 0, 0, 0))
    draw = ImageDraw.Draw(frame)
    for walker in sorted(walkers, key=lambda w: w.uid):
        left, right = sprites[walker.uid]
        image = right if walker.direction > 0 else left
        x = round(walker.x)
        top = height - image.height + round(walker.y + walker.hop)
        frame.paste(image, (x, top), image)

        if walker.shiny and walker.state != "drop":
            # Each shiny one twinkles for half a second every two seconds,
            # at a place on its sprite that changes every time.
            cycle = time + walker.uid * 0.7
            moment = cycle % 2.0
            if moment < 0.5:
                seed = int(cycle // 2.0) * 7919 + walker.uid
                sx = x + seed % max(1, image.width)
                sy = top + (seed // 7) % max(1, image.height // 2)
                _sparkle(draw, sx, sy, 1 if moment < 0.15 or moment > 0.35 else 2)
    return frame

