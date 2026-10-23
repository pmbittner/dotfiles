"""A tiny engine for small animated strips, independent of the Pokémon.

A scene holds entities on a stage. The loop (loop.py) asks the scene for
input, updates the entities, lets them draw a frame and hands the frame to
an output. Updating changes state; drawing only reads it.
"""

from .loop import run
from .output import Output, PngFile
from .scene import Entity, Scene, Stage
from .sprite import Sprite, load_sprite
from .walker import Walker

__all__ = ["Entity", "Output", "PngFile", "Scene", "Sprite", "Stage", "Walker", "load_sprite", "run"]
