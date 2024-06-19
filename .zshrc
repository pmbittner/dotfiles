

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
plugins=(git zsh-fs-navigation)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacsclient --reuse-frame --alternate-editor=emacs'
fi

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

### Default programs
TERMINAL=kitty
EXPLORER=dolphin

export HISTORY_IGNORE="(ls|cd|exit|cd ..)"

# typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='⟩⟩＝'
typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='>>='
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='λ'
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='🐢'
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='🍺'

reload() {
  clear
  source $HOME/.zshrc
}

## Shortcuts for directories
export A="$HOME/projects/AgdaCCnOC"

## setup for xserver
#export DISPLAY=$(ip route list default | awk '{print $3}'):0
#export DISPLAY=:0
#export LIBGL_ALWAYS_INDIRECT=1

## TeX Live
export MANPATH=/usr/local/texlive/2024/texmf-dist/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2024/texmf-dist/doc/info$INFOPATH
export PATH=/usr/local/texlive/2024/bin/x86_64-linux:$PATH

## my config setup
## I made this setup according to this instruction: https://www.atlassian.com/git/tutorials/dotfiles
export MYCONFIGDIR=$HOME/.myconfig.git
config() {
  /usr/bin/git --git-dir=$MYCONFIGDIR/ --work-tree=$HOME "$@"
}
config-add() {
  config add --patch
}

## tex
export PATH=/usr/local/texlive/2024/bin/x86_64-linux:$PATH

## for agda
export AGDA_DIR="$A/libs"

### (DOOM) EMACS SETUP
export PATH=~/.config/emacs/bin:$PATH
export DOOMDIR=$HOME/.config/doom
source $HOME/.emacsrc

## aliases
alias ls="ls --color=auto --group-directories-first"

alias checkout="git checkout"
alias commit="git commit"
alias pull="git pull"
alias push="git push"
alias stash="git stash"
alias log="git log --graph --oneline --color"
alias submodules-init="git submodule update --init --recursive"
alias submodules-update="git submodule update"

exp() {
  ${EXPLORER} . &
  disown
}
alias dol="exp"

fork() {
  ${TERMINAL} . &
  disown
}

alias u="cd .."

ev() {
  evince "$@" &
  disown
}
ok() {
  okular "$@" &
  disown
}
# alias to remain in ranger's directory after exiting ranger
alias ranger="source ranger"
alias r="ranger"

alias c="code"

# h for "home"
alias h="cd ~"
alias connect="nmcli con up"
alias disconnect="nmcli con down"

alias haskell-new-project="cabal init --interactive"
alias dnd="blobdrop -b -f gui"

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
[ -f "/home/paul/.ghcup/env" ] && . "/home/paul/.ghcup/env" # ghcup-env
