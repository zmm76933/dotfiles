# Vim-like keybind as default
#bindkey -v
# emacs-like keybind
bindkey -e
# Vim-like escaping jj keybind
#bindkey -M viins 'jj' vi-cmd-mode

# Add emacs-like keybind to viins mode
#bindkey -M viins '^F'  forward-char
#bindkey -M viins '^B'  backward-char
#bindkey -M viins '^P'  up-line-or-history
#bindkey -M viins '^N'  down-line-or-history
#bindkey -M viins '^A'  beginning-of-line
#bindkey -M viins '^E'  end-of-line
#bindkey -M viins '^K'  kill-line
#bindkey -M viins '^R'  history-incremental-pattern-search-backward
#bindkey -M viins '\er' history-incremental-pattern-search-forward
#bindkey -M viins '^Y'  yank
#bindkey -M viins '^W'  backward-kill-word
#bindkey -M viins '^U'  backward-kill-line
#bindkey -M viins '^H'  backward-delete-char
#bindkey -M viins '^?'  backward-delete-char
#bindkey -M viins '^G'  send-break
#bindkey -M viins '^D'  delete-char-or-list

#bindkey -M vicmd '^A'  beginning-of-line
#bindkey -M vicmd '^E'  end-of-line
#bindkey -M vicmd '^K'  kill-line
#bindkey -M vicmd '^P'  up-line-or-history
#bindkey -M vicmd '^N'  down-line-or-history
#bindkey -M vicmd '^Y'  yank
#bindkey -M vicmd '^W'  backward-kill-word
#bindkey -M vicmd '^U'  backward-kill-line
#bindkey -M vicmd '/'   vi-history-search-forward
#bindkey -M vicmd '?'   vi-history-search-backward

#bindkey -M vicmd 'gg' beginning-of-line
#bindkey -M vicmd 'G'  end-of-line

# removed keys(for miss typing)
# I want to bind good function for these keys!!
bindkey -r '^J'

# Like bash C-u behavior
bindkey '^U' backward-kill-line

# Mode/Delete like emacs symbol one
bindkey '^[^B' vi-backward-blank-word
bindkey '^[^F' vi-forward-blank-word
bindkey '^[^U' backward-delete-word
bindkey '^[[3~' delete-char
bindkey '^[^K' delete-word

if is-at-least 5.0.8; then
    autoload -Uz surround
    zle -N delete-surround surround
    zle -N change-surround surround
    zle -N add-surround surround
    bindkey -a cs change-surround
    bindkey -a ds delete-surround
    bindkey -a ys add-surround
    bindkey -a S add-surround
fi

# bind P and N for EMACS mode
#has 'history-substring-search-up' &&
#    bindkey -M emacs '^P' history-substring-search-up
#has 'history-substring-search-down' &&
#    bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
#has 'history-substring-search-up' &&
#    bindkey -M vicmd 'k' history-substring-search-up
#has 'history-substring-search-down' &&
#    bindkey -M vicmd 'j' history-substring-search-down

# bind P and N keys
has 'history-substring-search-up' &&
    bindkey '^P' history-substring-search-up
has 'history-substring-search-down' &&
    bindkey '^N' history-substring-search-down

# Insert a last word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word
#bindkey -M viins '^]' insert-last-word

# Surround a forward word by single quote
quote-previous-word-in-single() {
    modify-current-argument '${(qq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N quote-previous-word-in-single
bindkey '^Q' quote-previous-word-in-single
#bindkey -M viins '^Q' quote-previous-word-in-single

# Surround a forward word by double quote
quote-previous-word-in-double() {
    modify-current-argument '${(qqq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N quote-previous-word-in-double
bindkey '^Xq' quote-previous-word-in-double
#bindkey -M viins '^Xq' quote-previous-word-in-double

bindkey "$terminfo[kcbt]" reverse-menu-complete
#bindkey -M viins "$terminfo[kcbt]" reverse-menu-complete

#bindkey -s 'vv' "!vi\n"
#bindkey -s ':q' "^A^Kexit\n"

#
# functions
#
_delete-char-or-list-expand() {
    if [ -z "$RBUFFER" ]; then
        zle list-expand
    else
        zle delete-char
    fi
}
zle -N _delete-char-or-list-expand
bindkey '^D' _delete-char-or-list-expand

# CTRL-R - Paste the selected command from history into the command line
_fzf_use_tmux() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

_fzfcmd() {
  _fzf_use_tmux &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
  selected=( $(fc -rl 1 |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(_fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

insert-fzf-path-in-command-line() {
        local selected_path
        echo # Run fzf underneath the current prompt
        selected_path=$(ag . -l -g '' | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS" $(_fzfcmd)) || return
        LBUFFER="$LBUFFER${(q)selected_path} " # ${(q)VAR} shell-escapes the string
        zle reset-prompt
}
zle     -N insert-fzf-path-in-command-line
bindkey "^S" "insert-fzf-path-in-command-line"

_start-tmux-if-it-is-not-already-started() {
    BUFFER="${${${(M)${+commands[tmuxx]}#1}:+tmuxx}:-tmux}"
    if has "tmux_automatically_attach"; then
        BUFFER="tmux_automatically_attach"
    fi
    CURSOR=$#BUFFER
    zle accept-line
}
zle -N _start-tmux-if-it-is-not-already-started
if ! is_tmux_runnning; then
    bindkey '^T' _start-tmux-if-it-is-not-already-started
fi

do-enter() {
    if [[ -n $BUFFER ]]; then
        zle accept-line
        return $status
    fi

    : ${ls_done:=false}
    : ${git_ls_done:=false}

    if [[ $PWD != $GIT_OLDPWD ]]; then
        git_ls_done=false
    fi

    echo
    if is_git_repo; then
        if $git_ls_done; then
            if [[ -n $(git status --short) ]]; then
                git status
            fi
        else
            ${=aliases[ls]} && git_ls_done=true
            GIT_OLDPWD=$PWD
        fi
    else
        if [[ $PWD != $OLDPWD ]] && ! $ls_done; then
            ${=aliases[ls]} && ls_done=true
        fi
    fi

    zle reset-prompt
}
zle -N do-enter
bindkey '^m' do-enter

peco-select-gitadd() {
    local selected_file_to_add
    selected_file_to_add="$(
    git status --porcelain \
        | perl -pe 's/^( ?.{1,2} )(.*)$/\033[31m$1\033[m$2/' \
        | fzf --ansi --exit-0 \
        | awk -F ' ' '{print $NF}' \
        | tr "\n" " "
    )"

    if [ -n "$selected_file_to_add" ]; then
        BUFFER="git add $selected_file_to_add"
        CURSOR=$#BUFFER
        zle accept-line
    fi
    zle reset-prompt
}
zle -N peco-select-gitadd
bindkey '^g^a' peco-select-gitadd

exec-oneliner() {
    local oneliner_f
    oneliner_f="${ONELINER_FILE:-~/.commnad.list}"

    [[ ! -f $oneliner_f || ! -s $oneliner_f ]] && return

    local cmd q k res accept
    while accept=0; cmd="$(
        cat <$oneliner_f \
            | sed -e '/^#/d;/^$/d' \
            | perl -pe 's/^(\[.*?\]) (.*)$/$1\t$2/' \
            | perl -pe 's/(\[.*?\])/\033[31m$1\033[m/' \
            | perl -pe 's/^(: ?)(.*)$/$1\033[30;47;1m$2\033[m/' \
            | perl -pe 's/^(.*)([[:blank:]]#[[:blank:]]?.*)$/$1\033[30;1m$2\033[m/' \
            | perl -pe 's/(!)/\033[31;1m$1\033[m/' \
            | perl -pe 's/(\|| [A-Z]+ [A-Z]+| [A-Z]+ )/\033[35;1m$1\033[m/g' \
            | fzf --ansi --multi --no-sort --tac --query="$q" \
            --print-query --expect=ctrl-v --exit-0
            )"; do
        q="$(head -1 <<< "$cmd")"
        k="$(head -2 <<< "$cmd" | tail -1)"
        res="$(sed '1,2d;/^$/d;s/[[:blank:]]#.*$//' <<< "$cmd")"
        [ -z "$res" ] && continue
        if [ "$k" = "ctrl-v" ]; then
            vim "$oneliner_f" < /dev/tty > /dev/tty
        else
            cmd="$(perl -pe 's/^(\[.*?\])\t(.*)$/$2/' <<<"$res")"
            if [[ $cmd =~ "!$" || $cmd =~ "! *#.*$" ]]; then
                accept=1
                cmd="$(sed -e 's/!.*$//' <<<"$cmd")"
            fi
            break
        fi
    done

    local len
    if [[ -n $cmd ]]; then
        BUFFER="$(tr -d '@' <<<"$cmd" | perl -pe 's/\n/; /' | sed -e 's/; $//')"
        len="${cmd%%@*}"
        CURSOR=${#len}
        if [[ $accept -eq 1 ]]; then
            zle accept-line
        fi
    fi
    #zle reset-prompt
    zle redisplay
}
zle -N exec-oneliner
bindkey '^x^x' exec-oneliner

# expand global aliases by space
# http://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
globalias() {
  if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
    zle _expand_alias
    # zle expand-word
  fi
  zle self-insert
}

zle -N globalias

bindkey " " globalias

autobackq() {
    LBUFFER+='`'
    RBUFFER='`'"$RBUFFER"
}
zle -N autobackq
bindkey '`' autobackq

autobrace() {
    LBUFFER+="{"
    RBUFFER="}$RBUFFER"
}
zle -N autobrace
bindkey "{" autobrace

autobract() {
    LBUFFER+="["
    RBUFFER="]$RBUFFER"
}
zle -N autobract
bindkey "[" autobract

autodoubq() {
    LBUFFER+='"'
    RBUFFER='"'"$RBUFFER"
}
zle -N autodoubq
bindkey '"' autodoubq

autoparen() {
    LBUFFER+="("
    RBUFFER=")$RBUFFER"
}
zle -N autoparen
bindkey "(" autoparen

autosingq() {
    LBUFFER+="'"
    RBUFFER="'$RBUFFER"
}
zle -N autosingq
# bindkey "'" autosingq
