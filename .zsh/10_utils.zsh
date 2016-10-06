# Check whether the vital file is loaded
if ! vitalize 2>/dev/null; then
    echo "cannot run as shell script" 1>&2
    return 1
fi

# has_command returns true if $1 as a shell command exists
has.command() {
    (( $+commands[${1:?too few argument}] ))
    return $status
}

# has_command returns true if $1 as a shell function exists
has.function() {
    (( $+functions[${1:?too few argument}] ))
    return $status
}

# has_command returns true if $1 as a builtin command exists
has.builtin() {
    (( $+builtins[${1:?too few argument}] ))
    return $status
}

# has_command returns true if $1 as an alias exists
has.alias() {
    (( $+aliases[${1:?too few argument}] ))
    return $status
}

# has_command returns true if $1 as an alias exists
has.galias() {
    (( $+galiases[${1:?too few argument}] ))
    return $status
}

# has returns true if $1 exists
has() {
    has.function "$1" || \
        has.command "$1" || \
        has.builtin "$1" || \
        has.alias "$1" || \
        has.galias "$1"

    return $status
}

# zload is a helper function to autoload
# Example 1 : zload ~/work/function/_f
# Example 2 : zload *
# thanks @mollifier
zload() {
    if [[ $# -le 0 ]]; then
        echo "Usage: $0 PATH..." 1>&2
        echo "Load specified files as an autoloading function" 1>&2
        return 1
    fi

    local file function_path function_name
    for file in "$@"
    do
        if [[ -z $file ]]; then
            continue
        fi

        function_path="${file:h}"
        function_name="${file:t}"

        if (( $+functions[$function_name] )); then
            # "function_name" is defined
            unfunction "$function_name"
        fi
        FPATH="$function_path" autoload -Uz +X "$function_name"

        if [[ $function_name == _* ]]; then
            # "function_name" is a completion script

            # fpath requires absolute path
            # convert relative path to absolute path with :a modifier
            fpath=("${function_path:a}" $fpath) compinit
        fi
    done
}

# reload resets Completion function
reload() {
    local f
    f=(~/.zsh/Completion/*(.))
    unfunction $f:t 2>/dev/null
    autoload -U $f:t
}

# chpwd function is called after cd command
chpwd() {
    ls_abbrev
}
ls_abbrev() {
    # -a : Do not ignore entries starting with ..
    # -C : Force multi-column output.
    # -F : Append indicator (one of */=>@|) to entries.
    local cmd_ls='ls'
    local -a opt_ls
    opt_ls=('-CF' '--color=always')
    case "${OSTYPE}" in
        freebsd*|darwin*)
            if type gls > /dev/null 2>&1; then
                cmd_ls='gls'
            else
                # -G : Enable colorized output.
                opt_ls=('-CFG')
            fi
            ;;
    esac

    local ls_result
    ls_result=$(CLICOLOR_FORCE=1 COLUMNS=$COLUMNS command $cmd_ls ${opt_ls[@]} | sed $'/^\e\[[0-9;]*m$/d')

    local ls_lines=$(echo "$ls_result" | wc -l | tr -d ' ')

    if [ $ls_lines -gt 5 ]; then
        echo "$ls_result" | head -n 5
        echo '...'
        echo "$(command ls -1 -A | wc -l | tr -d ' ') files exist"
    else
        echo "$ls_result"
    fi
}

256colortest() {
    local code
    for code in {0..255}
    do
        echo -e "\e[38;05;${code}m $code: Test"
    done
}
