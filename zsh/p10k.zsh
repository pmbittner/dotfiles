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
