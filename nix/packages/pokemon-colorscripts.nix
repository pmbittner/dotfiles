{
  stdenvNoCC,
  fetchFromGitLab,
  makeWrapper,
  python3,
}:
stdenvNoCC.mkDerivation {
  pname = "pokemon-colorscripts";
  version = "1.0";
  src = fetchFromGitLab {
    owner = "phoneybadger";
    repo = "pokemon-colorscripts";
    rev = "5802ff67520be2ff6117a0abc78a08501f6252ad";
    hash = "sha256-gKVmpHKt7S2XhSxLDzbIHTjJMoiIk69Fch202FZffqU=";
  };
  buildInputs = [python3];
  installPhase = ''
    mkdir -p "$out/bin"
    mkdir -p "$out/share"
    cp -rf colorscripts "$out/share"
    cp pokemon-colorscripts.py "$out/share"
    cp pokemon.json "$out/share"
    ln -s "$out/share/pokemon-colorscripts.py" "$out/bin/pokemon-colorscripts"
  '';
}
