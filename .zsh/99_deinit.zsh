printf "\n"
printf "$fg_bold[cyan] This is ZSH $fg_bold[red]$ZSH_VERSION"
printf "$fg_bold[cyan] - DISPLAY on $fg_bold[red]$DISPLAY$reset_color\n\n"

# ruby version control
eval "$(rbenv init - zsh)"
eval "$(pyenv init - zsh)"
