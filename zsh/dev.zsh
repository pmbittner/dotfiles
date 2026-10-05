# Development
# Programming languages and tools.
# Sourced by ~/.zshrc.

## for agda
# export A="$HOME/projects/AgdaCCnOC"
# export AGDA_DIR="$A/libs"

alias c="code"

alias haskell-new-project="cabal init --interactive"

pb-jdtls-delete-cache () {
  rm -rf /tmp/jdtls*
}

#### Python
venv-activate () {
  source "$@/bin/activate"
}
venv-deactivate () {
  deactivate
}
alias v=venv-activate
alias vd=venv-deactivate
