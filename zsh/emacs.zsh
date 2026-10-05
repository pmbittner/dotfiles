# (Doom) Emacs
# Paths and helpers for Doom Emacs; the start helpers are in ~/.emacsrc.
# Sourced by ~/.zshrc.

### (DOOM) EMACS SETUP
export EMACSDIR=~/.config/emacs
export DOOMDIR=~/.config/doom
export PATH=$EMACSDIR/bin:$PATH

source ~/.emacsrc

pb-emacs-fix-config () {
  vim $DOOMDIR/config.el
}

# run this when aspell highlights every word as incorrect in Emacs
pb-spell-fu-delete-cache () {
  rm -rf $EMACSDIR/.local/etc/spell-fu/*
}
