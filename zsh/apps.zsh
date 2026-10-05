# Applications
# Shortcuts to start applications.
# Sourced by ~/.zshrc.

chrome () {
  nix-shell -p ungoogled-chromium --run chromium
}

ev() {
  evince "$@" &
  disown
}
okular() {
  nix-shell -p "okular" --run "okular $@" &
  disown
}

alias dnd="blobdrop -b -f gui"

zoom() {
  env XDG_CURRENT_DESKTOP=gnome zoom "$@"
}

## colorkiste
colourkiste() {
  (cd "${HOME}/projects/ColourKiste"; java -Dsun.java2d.uiScale=1.5 -jar "target/ColourKiste-1.0-SNAPSHOT-jar-with-dependencies.jar")
}
alias colorkiste=colourkiste
alias ck=colorkiste
