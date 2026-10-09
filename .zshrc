## OS information
if [[ $(uname) == "Darwin" ]]; then
  macos=true

  # include python modules
  export PATH=$HOME/.local/bin:$PATH
else
  macos=false
fi

## fancy colors to greet me (gitlab.com/dwt1/shell.color-scripts)
#colorscript exec pinguco #space-invaders six random
#colorscript random
alias pkmn="pokemon-colorscripts"
pkmn --no-title --random 1-4

########## DEFAULT ZSH STUFF BELOW ###############################

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# oh-my-zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(git sudo extract fancy-ctrl-z nix-shell zsh-fs-navigation zsh-syntax-highlighting)
# ctrl-f: open current command in editor
# ctrl-z: fancy-ctrl-z

source $ZSH/oh-my-zsh.sh

## Prompt (powerlevel10k)
source ~/zsh/p10k.zsh

########## CUSTOM ADDITIONS BY ME (PAUL) ###################

### Default programs
TERMINAL=kitty
EXPLORER=thunar

export HISTORY_IGNORE="(ls|cd|exit|cd ..)"

reload() {
  clear
  source $HOME/.zshrc
}

pb-open () {
  if $macos; then
    open "$@"
  else
    xdg-open "$@"
  fi
}

## Topic files in ~/zsh/
for pb_zsh_file in git nix emacs files media system apps dev; do
  source "$HOME/zsh/${pb_zsh_file}.zsh"
done
unset pb_zsh_file

## MacOS (sourced last, so it can override the settings above)
if $macos; then
  source ~/zsh/mac.zsh
fi

## direnv
command -v direnv >/dev/null && eval "$(direnv hook zsh)"

### Include any commands that only work on the local machine
[[ ! -f ~/.local.zsh ]] || source ~/.local.zsh

## high dpi wsl settings
# export GDK_SCALE=0.5
# export GDK_DPI_SCALE=2

if $macos; then
  if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  fi

  if [ -d "${HOME}/.ghcup" ]; then
    PATH=$HOME/.ghcup/bin:$PATH
  fi
else
  PBUSERNAME="$(whoami)"
  if [ -e /home/${PBUSERNAME}/.nix-profile/etc/profile.d/nix.sh ]; then . /home/${PBUSERNAME}/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
  [ -f "/home/${PBUSERNAME}/.ghcup/env" ] && . "/home/${PBUSERNAME}/.ghcup/env" # ghcup-env
fi
