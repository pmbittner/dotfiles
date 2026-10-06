# Git
# Bare dotfiles repository (config) and git shortcuts.
# Sourced by ~/.zshrc.

## my config setup
## I made this setup according to this instruction: https://www.atlassian.com/git/tutorials/dotfiles
export MYCONFIGDIR=$HOME/.myconfig.git
config() {
  git --git-dir=$MYCONFIGDIR/ --work-tree=$HOME "$@"
}
config-add() {
  config add --patch
}

alias sw="git switch"
alias br="git branch"
alias gpfn="gpf --no-verify"
alias commit="git commit"
alias pull="git pull"
alias push="git push"
alias stash="git stash"
alias log="git log --graph --oneline --color"
alias pb-git-submodules-init="git submodule update --init --recursive"
alias pb-git-submodules-update="git submodule update"
alias pb-git-submodules-add="git submodule add"
alias pb-git-fetchallbranches='git branch -r | grep -v "\->" | sed "s,\x1B\[[0-9;]*[a-zA-Z],,g" | while read remote; do git branch --track "${remote#origin/}" "$remote"; done'

pb-git-stash-abort() {
  git reset --merge
}

pb-git-undo-last-commit () {
  git reset HEAD~
}
