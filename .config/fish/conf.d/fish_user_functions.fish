function dired
    emacsclient -e "(dired \"$PWD\")"
end

function cde
    emacsclient -e "(editutil-current-buffer-directory)" | sed 's/^"\(.*\)"$/\1/' | read EMACS_CWD
    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
end

function cdv
    if test -n "$TMPDIR"
        set -l sockets (find $TMPDIR /tmp -type s -name "nvim*" 2>/dev/null)
        set -l NVIM_CWD (nvim --server "$sockets[1]" --remote-expr "expand('%:p:h')" 2>&1 >/dev/null)
        echo "chdir to $NVIM_CWD"
        cd "$NVIM_CWD"
    end
end

function fzf_z
    set -l query (commandline)

    if test -n "$query"
        set fzf_flags --query "$query"
    end

    z -l | awk '{ print $2 }' | fzf $fzf_flags | read recent
    if test "$recent"
        cd "$recent"
        commandline -r ''
        commandline -f repaint
    end
end

function ssh
    if test -n "$TMUX"
        set -l pane_id (tmux display -p '#{pane_id}')
        command ssh "$argv"
        tmux select-pane -t "$pane_id" -P default
    else
        command ssh "$argv"
    end
end
