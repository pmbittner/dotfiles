# Powerlevel10k prompt
# Loads the generated prompt config (~/.p10k.zsh) and our changes to it.
# Sourced by ~/.zshrc right after oh-my-zsh, which loads the theme itself
# (ZSH_THEME). The instant prompt has to stay at the top of ~/.zshrc.

# To customize the prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

### Our changes to the generated config
# typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='⟩⟩＝'
typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='>>='
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='λ'
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='🐢'
# typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='🍺'

### Colors from the desktop theme (NixOS only)
# Prompt colors given as numbers 0-15 come from the terminal's palette. On
# NixOS, kitty's palette is generated from the desktop theme (see
# docs/STYLE.md), so these colors follow theme switches. The generated
# ~/.p10k.zsh also uses a few fixed colors from the 256-color range, which
# look the same in every theme. On NixOS, they are replaced by palette
# colors here; on other systems the prompt stays as generated.
# Color 0 is the theme's background, which works as text color on all
# colored segments, in light and dark themes alike.
if [[ -e /etc/NIXOS ]]; then   # this file exists on every NixOS system
  typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_GAP_FOREGROUND=8 # 242
  typeset -g POWERLEVEL9K_OS_ICON_FOREGROUND=0                    # 232
  typeset -g POWERLEVEL9K_DIR_FOREGROUND=0                        # 254
  typeset -g POWERLEVEL9K_DIR_SHORTENED_FOREGROUND=0              # 250
  typeset -g POWERLEVEL9K_DIR_ANCHOR_FOREGROUND=0                 # 255
  typeset -g POWERLEVEL9K_TIMEWARRIOR_FOREGROUND=0                # 255
  typeset -g POWERLEVEL9K_GO_VERSION_FOREGROUND=0                 # 255
  typeset -g POWERLEVEL9K_PERLBREW_FOREGROUND=0                   # 67
  typeset -g POWERLEVEL9K_ASDF_RUST_BACKGROUND=3                  # 208 (orange)
  typeset -g POWERLEVEL9K_RUST_VERSION_BACKGROUND=3               # 208 (orange)
  typeset -g POWERLEVEL9K_RVM_BACKGROUND=8                        # 240
fi

# Apply the changes above if the prompt is already running (e.g. after
# `reload`), like ~/.p10k.zsh does at its end.
(( ! $+functions[p10k] )) || p10k reload
