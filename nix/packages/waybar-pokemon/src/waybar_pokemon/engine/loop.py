"""The main loop: input, update, draw, present, a fixed number of times per second."""

import time

from PIL import Image

from .output import Output
from .scene import Scene

# Longest step that is simulated, in seconds. After a longer pause (e.g. a
# suspended computer) the entities continue where they were.
MAX_STEP = 0.5


def run(scene: Scene, output: Output, fps: float) -> None:
    period = 1.0 / fps
    last = time.monotonic()
    while True:
        now = time.monotonic()
        dt, last = min(now - last, MAX_STEP), now

        scene.sync()
        scene.update(dt)
        frame = Image.new("RGBA", (scene.stage.width, scene.stage.height), (0, 0, 0, 0))
        scene.draw(frame)
        output.present(frame)

        time.sleep(max(0.0, period - (time.monotonic() - now)))
