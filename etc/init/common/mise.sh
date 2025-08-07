#!/bin/bash

# Stop script if errors occur
# trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
# set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if is_linux; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if is_macos; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# exit with true if you have mise command
if ! has "mise"; then
    log_fail "error: this script is only supported with mise"
    exit 1
fi

# Set up shell completions
if has "fish"; then
    mise completion fish > ${HOME}/.config/fish/completions/mise.fish
fi

plugins=(
    'gh'
    'ghq'
    'go'
    'node'
    'python'
    'perl'
    'ruby'
    'rust'
    'terraform'
    'uv'
)

for index in ${!plugins[*]}
do
    plugin=$(echo ${plugins[$index]} | cut -d' ' -f 1)
    mise use -g ${plugin}@latest
done

log_pass "mise: installed successfully"
