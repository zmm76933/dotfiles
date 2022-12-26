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

if is_macos && is_arm; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# exit with true if you have asdf command
if ! has "asdf"; then
    log_fail "error: this script is only supported with asdf"
    exit 1
fi

plugins=(
    'direnv'
    'python'
    'poetry'
    'ruby'
    'nodejs'
    'perl'
)

for index in ${!plugins[*]}
do
    plugin=$(echo ${plugins[$index]} | cut -d' ' -f 1)
    asdf plugin add ${plugin}
    if [ $? -eq 2 ]; then
      continue
    fi

    asdf install ${plugin} latest
    asdf global ${plugin} latest
done

log_pass "asdf: installed successfully"
