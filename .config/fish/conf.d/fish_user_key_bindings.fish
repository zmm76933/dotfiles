function fish_user_key_bindings
  # default key bindings
  bind \cg cancel
  bind \ef forward-word
  bind \ec capitalize-word

  # prevent iterm2 from closing when typing Ctrl-D (EOF)
  bind \cd delete-char

  # git repository search
  bind \cs __ghq_repository_search
end
