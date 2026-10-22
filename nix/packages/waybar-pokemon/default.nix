{ lib, python3Packages }:
python3Packages.buildPythonApplication {
  pname = "waybar-pokemon";
  version = "0.1.0";
  pyproject = true;

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./pyproject.toml
      ./README.md
      ./waybar_pokemon
    ];
  };

  build-system = [ python3Packages.setuptools ];
  dependencies = [ python3Packages.pillow ];

  meta = {
    description = "Little Pokémon walking around in a waybar module";
    mainProgram = "waybar-pokemon";
    platforms = lib.platforms.linux;
  };
}
