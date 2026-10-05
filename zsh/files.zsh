# Files and navigation
# Listing, opening, searching and handling files and directories.
# Sourced by ~/.zshrc.

## aliases
if $macos; then
  alias ls="ls -Gaph"
else
  alias ls="ls -a --color=auto --group-directories-first"
fi

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

## Fuzzy finder to change the current directory to the directory of a file.
## This searches in the current directory.
f() {
  file_path=$(sk)
  pb-open "${file_path}"
}
## This searches from home directory.
F() {
  ## sk always searches files from the current directory.
  ## Since we want to search all files, we have to go to the root directory first.
  curdir=${PWD}
  cd ~
  file_path=$(sk)
  if [ -z "${file_path}" ]; then
    # Search was aborted in sk. Go back to where we started.
    cd ${curdir}
  else
    # The user selected a file in the fuzzy search.
    # Open it with preferred program
    # We might want to consider opening emacs instead (of course).
    pb-open "${file_path}"
  fi
}
## Fuzzy finder to change the current directory to the directory of a file.
## This searches in the current directory.
fcd() {
  file_path=$(sk)
  dir=$(dirname "${file_path}")
  cd "${dir}"
}
## This searches from home directory.
Fcd() {
  ## sk always searches files from the current directory.
  ## Since we want to search all files, we have to go to the root directory first.
  curdir=${PWD}
  cd ~
  file_path=$(sk)
  if [ -z "${file_path}" ]; then
    # Search was aborted in sk. Go back to where we started.
    cd ${curdir}
  else
    # The user selected a file in the fuzzy search.
    # Go to the directory of that file.
    # We might want to consider opening emacs instead (of course).
    dir=$(dirname "${file_path}")
    cd "${dir}"
  fi
}
alias fdir="fcd"
alias Fdir="Fcd"

# find a text in all files at current directory
ft () {
  rg -e "$@" .
}

# alias to remain in ranger's directory after exiting ranger
ranger_cd () {
  temp_file=$(mktemp)
  \ranger --choosedir="$temp_file" "$@"  # Run ranger and store the last visited directory in temp_file
  if [ -f "$temp_file" ]; then
    last_dir=$(cat "$temp_file")       # Read the last directory from temp_file
    rm -f "$temp_file"                # Remove the temp_file
    if [ -d "$last_dir" ]; then
      cd "$last_dir"                # Change to the last directory if it exists
    fi
  else
    echo "Cannot go to ranger's visited location!"
  fi
}
alias ranger="ranger_cd"
alias r="ranger"

# h for "home"
alias h="cd ~"

pb-file-sizes () {
  du -sh "$@"
}

pb-untargz () {
  tar -xf "$@"
}

# argument is a suffix that should be appended to all file names
pb-append-to-all-file-names () {
  find . -type f ! -name "*$@" -exec sh -c "mv '{}' '{}$@'" \;
}
