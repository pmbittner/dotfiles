

## fancy colors to greet me (gitlab.com/dwt1/shell.color-scripts)
colorscript exec pinguco #space-invaders six random
#colorscript random

########## DEFAULT ZSH STUFF BELOW ###############################

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


########## CUSTOM ADDITIONS BY p10k configure ###################

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

########## CUSTOM ADDITIONS BY ME (PAUL) ###################

export HISTORY_IGNORE="(ls|cd|exit|cd ..)"

## Shortcuts for directories
export V="$HOME/VariantSync"
export A="$HOME/projects/AgdaCCnOC"
export P="$HOME/paper/expressive-power-of-variability-languages/paper"
export C="$HOME/teaching/compiler-construction/material/2023ws"
export CP="$HOME/teaching/compiler-construction/tasks"
export CP1="$CP/task1"
export CP2="$CP/task2"

## setup for xserver
export DISPLAY=$(ip route list default | awk '{print $3}'):0
#export DISPLAY=:0
export LIBGL_ALWAYS_INDIRECT=1

## my config setup
## I made this setup according to this instruction: https://www.atlassian.com/git/tutorials/dotfiles
export MYCONFIGDIR=$HOME/.myconfig.git
config() {
  /usr/bin/git --git-dir=$MYCONFIGDIR/ --work-tree=$HOME "$@"
}

## for agda
export AGDA_DIR="$A/libs"
[ -f "/home/bittner/.ghcup/env" ] && source "/home/bittner/.ghcup/env" # ghcup-env

### EMACS SERVER SETUP BEGIN
# We use the following file as a variable to toggle whether we run emacs as a standalone tool or as a client/server.
DISABLE_EMACS_SERVER_USAGE_FILE="$HOME/.local/state/paul_disables_emacs_server_usage"
emacs-start-server() {
  pgrep emacs > /dev/null || /usr/local/bin/emacs --daemon
}
emacs-kill-server() {
  pkill emacs
}
emacs-restart-server() {
  emacs-kill-server
  sleep 1
  emacs-start-server
}
emacs-toggle-server() {
  if [ -f "$DISABLE_EMACS_SERVER_USAGE_FILE" ]
  then
    rm "$DISABLE_EMACS_SERVER_USAGE_FILE"
    echo "START emacs server mode"
    emacs-start-server
  else
    touch "$DISABLE_EMACS_SERVER_USAGE_FILE"
    echo "STOP emacs server mode. Running servers are terminated."
    emacs-kill-server
  fi
}
is-emacs-server() {
  if [ -f "$DISABLE_EMACS_SERVER_USAGE_FILE" ]
  then
    echo "No, emacs will be run as standalone."
  else
    echo "Yes, emacs will run as client."
  fi
}
emacs-sync() {
  emacs-kill-server
  doom sync
  if [ -f "$DISABLE_EMACS_SERVER_USAGE_FILE" ]
  then
  else
    emacs-start-server
  fi
}
alias eeserver='emacs-start-server'
alias ees='is-emacs-server'
alias eerestart='emacs-restart-server'
alias eekill='emacs-kill-server'
alias eetoggle='emacs-toggle-server'
alias eet='emacs-toggle-server'
alias eesync='emacs-sync'

ee() {
  if [ -f "$DISABLE_EMACS_SERVER_USAGE_FILE" ]
  then
    (cd "$@" ; emacs &!)
  else
    emacs-start-server
    emacsclient -c -a 'emacs' "$@" &!
  fi
}

### SHORTHANDS TO START EMACS FOR CERTAIN PROJECTS
alias eea='ee $A'
alias eev='ee ~/paper/variantsync-proposal'
alias eep='ee $P'
alias eec='ee $C'
alias eecp1='ee $CP1'
alias eecp2='ee $CP2'

### EMACS SERVER SETUP END

### DOOM EMACS SETUP BEGIN
export PATH=~/.emacs.d/bin:$PATH
export DOOMDIR=$HOME/.config/doom
### DOOM EMACS SETUP END

## aliases
alias ls="ls --color=auto --group-directories-first"

alias checkout="git checkout"
alias commit="git commit"
alias pull="git pull"
alias push="git push"
alias stat="git status"

alias explorer="explorer.exe"
alias exp="explorer.exe ."

alias u="cd .."

## colorkiste
colourkiste() {
  java -jar ~/software/ColourKiste.jar &
}
alias colorkiste=colourkiste
alias ck=colorkiste

## other utilities
shrink-all-pngs () {
  mkdir small
  find -maxdepth 1 -name "*.png" -exec convert {} -resize 2048x2048 small/{} \;
}
shrink-all-jpgs () {
  mkdir small
  find -maxdepth 1 -name "*.jpg" -exec convert {} -resize 2048x2048 small/{} \;
}

convert-pngs-to-jpgs () {
  find -maxdepth 1 -name "*.png" -exec convert {} {}.jpg \;
}

compose-pngs-to-pdf () {
  convert *.png -auto-orient composed.pdf
}
compose-jpgs-to-pdf () {
  convert *.jpg -auto-orient composed.pdf
}

scans-to-pdf () {
  shrink-all-pngs
  cd small
  convert-pngs-to-jpgs
  compose-jpgs-to-pdf
  mv composed.pdf ..
  cd ..
  rm -rf small
}

## high dpi wsl settings
# export GDK_SCALE=0.5
# export GDK_DPI_SCALE=2

### TODO add jetbrains font to dotfiles repo?

if [ -e /home/bittner/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bittner/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
