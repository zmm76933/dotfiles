function fish_user_key_bindings
    # default key bindings
    bind \cg cancel

    # prevent iterm2 from closing when typing Ctrl-D (EOF)
    bind \cd delete-char

    # move the word to the left of the cursor to the killring. The “word” here is everything up to punctuation or whitespace. 
    bind \cw backward-kill-word

    # recend directory search
    bind \cs fzf_z

    # git repository search
    bind \er __ghq_repository_search
end
