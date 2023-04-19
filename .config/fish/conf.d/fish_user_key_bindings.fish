function fish_user_key_bindings
  # default key bindings
  bind \cg cancel

  # prevent iterm2 from closing when typing Ctrl-D (EOF)
  bind \cd delete-char

  # recend directory search
  bind \cs fzf_recentd

  # git repository search
  bind \er __ghq_repository_search
end
