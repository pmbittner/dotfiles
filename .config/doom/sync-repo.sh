#!/usr/bin/env zsh

CYAN='\033[0;36m'
NC='\033[0m' # No Color

show-and-tell () {
    echo -e "${CYAN}> $@${NC}"
    "$@"
}

REPO="$1"
if [[ -d "$REPO" ]]; then
    cd "$REPO" || { echo "cd failed, exiting"; exit; }
    show-and-tell git commit -am 'commit via emacs'
    show-and-tell git pull -r && show-and-tell git push
else
    echo "Error: Given argument '$REPO' is not a directory!"
fi
